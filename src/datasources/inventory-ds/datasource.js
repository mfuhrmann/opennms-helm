import {ClientDelegate} from '../../lib/client_delegate';
import {API, Model} from '../../opennms';
import {FilterCloner} from '../../lib/query/FilterCloner';
import {Mapping} from './Mapping';
import _ from 'lodash';

const FeaturedAttributes = [
    'label',
    'foreignSource',
    'foreignId',
    'location',
    'ipAddress',
];

const isNumber = function isNumber(num) {
    return ((parseInt(num,10) + '') === (num + ''));
};

export class OpenNMSInventoryDatasource {

  constructor(instanceSettings, $q, backendSrv, templateSrv, contextSrv) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.q = $q;
    this.backendSrv = backendSrv;
    this.templateSrv = templateSrv;
    this.client = new ClientDelegate(instanceSettings, backendSrv, $q);

    // When enabled in the datasource, the grafana user should be used instead of the datasource username on
    // supported operations
    if (instanceSettings.jsonData && instanceSettings.jsonData.useGrafanaUser) {
        // If the datasource contains the field which should be used and that field is set, use it
        if (instanceSettings.jsonData.grafanaUserField && contextSrv.user[instanceSettings.jsonData.grafanaUserField]) {
            this.user = contextSrv.user[instanceSettings.jsonData.grafanaUserField];
        } else { // otherwise the login is used instead
            this.user = contextSrv.user.login;
        }
    }
  }

  query(options) {
      // Initialize filter
      var filter = options.targets[0].filter || new API.Filter();
      filter.limit = options.targets[0].limit || 0; // 0 = no limit

      options.enforceTimeRange = true;
      const clonedFilter = this.buildQuery(filter, options);

      var self = this;
      return this.client.findNodes(clonedFilter).then(nodes => {
          return this.client.getClientWithMetadata().then(client => {
              return {
                data: self.toTable(nodes, client.server.metadata)
              };
          });
      });
  }

  // Clone Filter to make substitution possible
  // (otherwise substitution would happen in original query,
  // and overwriting the $<variable> or [[variable]] in restrictions which may not be the intention)
  buildQuery(filter, options) {
      var clonedFilter = new FilterCloner().cloneFilter(filter);

      /*
      // Before replacing any variables, add a global time range restriction (which is hidden to the user)
      if (options && options.enforceTimeRange) {
          clonedFilter.withAndRestriction(
              new API.NestedRestriction()
                  .withAndRestriction(new API.Restriction("lastEventTime", API.Comparators.GE, "$range_from"))
                  .withAndRestriction(new API.Restriction("lastEventTime", API.Comparators.LE, "$range_to")));
      }
      */

      // Substitute $<variable> or [[variable]] in the restriction value
      this.substitute(clonedFilter.clauses, options);
      return clonedFilter;
  }

  _getTemplateVariable(name) {
    if (this.templateSrv.variables && this.templateSrv.variables.length > 0) {
        return this.templateSrv.variables.filter((v) => {
            return v.name === name;
        })[0];
    }
    return undefined;
  }

  subtituteNodeRestriction(clause) {
    const restriction = clause.restriction;
    // Handle "node" as a special case, updating restrictions to either foreignSource+foreignId or node.id
    if (restriction.attribute === 'node') {
        if (restriction.value.indexOf(':') > 0) {
            if (restriction.comparator.id !== API.Comparators.EQ.id) {
                console.log('WARNING: Using a comparator other than EQ will probably not work as expected with a foreignSource:foreignId node criteria.');
            }
            const nodeCriteria = restriction.value.split(':');
            const replacement = new API.NestedRestriction(
                new API.Clause(new API.Restriction('node.foreignSource', restriction.comparator, nodeCriteria[0]), API.Operators.AND),
                new API.Clause(new API.Restriction('node.foreignId', restriction.comparator, nodeCriteria[1]), API.Operators.AND),
            );
            clause.restriction = replacement;
        } else if (isNumber(restriction.value)) {
            clause.restriction = new API.Restriction('node.id', restriction.comparator, restriction.value);
        } else if (restriction.value === '{}') {
            return true;
        } else {
            console.log('WARNING: found a "node" criteria but it does not appear to be a node ID nor a foreignSource:foreignId tuple.',restriction);
        }
    } 
    return false;
  }

  substitute(clauses, options) {
      const self = this;
      const remove = [];
      _.each(clauses, clause => {
        if (clause.restriction) {
            const restriction = clause.restriction;
            if (restriction instanceof API.NestedRestriction) {
                self.substitute(restriction.clauses, options);
            } else if (restriction.value) {
                const variableName = self.templateSrv.getVariableName(restriction.value);
                const templateVariable = self._getTemplateVariable(variableName);

                // Process multi-selects
                if (templateVariable && templateVariable.multi) {
                    if (templateVariable.current.value && self.templateSrv.isAllValue(templateVariable.current.value)) {
                        // if we're querying "all" we just dump the clause altogether
                        remove.push(clause);
                    } else {
                        // annoyingly, depending on how you interact with the UI, if one value is selected it will
                        // *either* be an array with 1 entry, or just the raw value >:|
                        // so we normalize it back to just the raw value here if necessary
                        if (_.isArray(templateVariable.current.value) && templateVariable.current.value.length === 1) {
                            templateVariable.current.value = templateVariable.current.value[0];
                        }

                        // now if it's *still* an array, we chop it up into nested restrictions
                        if (_.isArray(templateVariable.current.value)) {
                            const replacement = new API.NestedRestriction();
                            let values = templateVariable.current.value;
                            if (!_.isArray(values)) {
                                values = [values];
                            }
                            for (const value of values) {
                                if (restriction.comparator.id === API.Comparators.EQ.id) {
                                    replacement.withOrRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value));
                                } else if (restriction.comparator.id === API.Comparators.NE.id) {
                                    replacement.withAndRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value));
                                } else {
                                    throw new Error('Unable to query "' + restriction.attribute + '": multi-select values with variable substitution must be either "=" or "!="');
                                }
                            }

                            // we've turned a single restriction into a nested one, so re-process it as a
                            // collection and skip the simple replacement below
                            clause.restriction = replacement;
                            self.substitute(clause.restriction.clauses, options);
                            return;
                        }
                    }
                }

                // Range must be of type date, otherwise it is not parseable by the OpenNMS client
                if (variableName === 'range_from') {
                    restriction.value = options.range.from;
                } else if (variableName === 'range_to') {
                    restriction.value = options.range.to;
                } else {
                    restriction.value = self.templateSrv.replace(restriction.value, options.scopedVars);
                }

                const shouldRemove = self.subtituteNodeRestriction(clause);
                if (shouldRemove) {
                    remove.push(clause);
                }
            }
        }
      });
      for (const r of remove) {
        const i = clauses.indexOf(r);
        if (i >= 0) {
            clauses.splice(i, 1);
        }
      }
    }

  testDatasource() {
      return this.client.getClientWithMetadata()
          .then(metadata => {
              if (metadata) {
                  return {
                      status: "success",
                      message: "Data source is working",
                      title: "Success"
                  };
              } else {
                return {
                  status: "danger",
                  message: "OpenNMS provided a response, but no metadata was found.",
                  title: "Unexpected Response"
                }
              }
          }).catch(e => {
              if (e.message === "Unsupported Version") {
                  return {
                      status: "danger",
                      message: "The OpenNMS version you are trying to connect to is not supported. " +
                               "OpenNMS Horizon version >= 20.1.0 or OpenNMS Meridian version >= 2017.1.0 is required.",
                      title: e.message
                  }
              } else {
                  throw e;
              }
          });
  }

  annotationQuery(/* options */) {
    return this.q.when([]);
  }

  metricFindQuery(query) {
    if (!query || !query.find) {
        return this.q.when([]);
    }

    if (query.find === "attributes") {
        if (query.strategy === 'featured') {
            const featuredAttributes = _.map(_.sortBy(FeaturedAttributes), (attribute) => {
                return {id: attribute}
            });
            return this.q.when(featuredAttributes);
        }
        // assume all
        return this.client.getProperties();
    }
    if (query.find === "comparators") {
        const attribute = new Mapping.AttributeMapping().getApiAttribute(query.attribute);
        return this.client.getPropertyComparators(attribute);
    }
    if (query.find == 'values') {
        return this.searchForValues(query);
    }
    if (query.find === 'operators') {
        return this.client.findOperators();
    }
    return this.q.when([]);
  }

  searchForValues(query) {
      let attribute = new Mapping.AttributeMapping().getApiAttribute(query.attribute);
      if (attribute === 'ipAddr') {
          attribute = 'ipInterface.ipAddress';
      }
      if (attribute === 'isSituation' || attribute === 'isInSituation') {
        return this.q.when([{ id: 'false', label: 'false'}, {id: 'true', label: 'true'}]);
      }
      return this.client.findProperty(attribute)
          .then(property => {
              if (!property) {
                  return this.q.when([]);
              }
              // Special handling for properties
              switch(property.id) {
                  // Severity is handled separately as otherwise the severity ordinal vs the severity label would be
                  // used, but that may not be ideal for the user
                  case 'severity':
                      return this.q.when(_.map(Model.Severities, severity => {
                          return {
                              id: severity.id,
                              label: severity.label
                          }
                      }));
              }
              return property.findValues({limit: 1000}).then(values => {
                  return values.map(value => {
                      return {id: value, label: value}
                  });
              });
          });
  }

    // Converts the data fetched from the Node REST Endpoint of OpenNMS to the grafana table model
    toTable(nodes /*, metadata */) {
        let columnNames = [
            "ID", "Label", "Label Source", "Foreign Source", "Foreign ID", "Location",
            "Creation Time", "Parent ID", "Parent Foreign Source", "Parent Foreign ID",
            "Type", "SNMP sysObjectID", "SNMP sysName", "SNMP sysDescription", "SNMP sysLocation",
            "SNMP sysContact", "NETBIOS/SMB Name", "NETBIOS/SMB Domain", "Operating System",
            "Last Poll Time", "Primary SNMP Physical Address", "Primary SNMP ifIndex", "Primary IP Interface", "Categories",
            "Data Source"
        ];

        let columnMappings = [
            'id',
            'label',
            'labelSource',
            'foreignSource',
            'foreignId',
            'location',
            'createTime',
            'parent.id',
            'parent.foreignSource',
            'parent.foreignId',
            'type',
            'sysObjectId',
            'sysName',
            'sysDescription',
            'sysLocation',
            'sysContact',
            'netBiosName',
            'netBiosDomain',
            'operatingSystem',
            'lastCapsdPoll',
            'ipInterface.snmpInterface.physAddr',
            'ipInterface.ifIndex',
            'ipInterface.ipAddress',
            'categories.name'
        ];

        /*
        interface Column {
            text: string;
            title?: string;
            type?: string;
            sort?: boolean;
            desc?: boolean;
            filterable?: boolean;
            unit?: string;
        }
        */

        let getPrimary = (node) => {
            if (node && node.ipInterfaces) {
                let primary = node.ipInterfaces.filter(iface => {
                    return iface.snmpPrimary && iface.snmpPrimary.isPrimary();
                })[0];
                return primary;
            }
            return undefined;
        };

        const columns = columnNames.map((col, index) => {
            const name = columnMappings[index];
            const chopped = name ? name.split('.') : [];
            return {
                name: chopped[chopped.length - 1],
                resource: name,
                text: col
            };
        });

        let self = this;
        let rows = _.map(nodes, node => {
            let primaryIpInterface = getPrimary(node);
            let primarySnmp = primaryIpInterface && primaryIpInterface.snmpInterface;

            let row = [
                node.id,
                node.label,
                node.labelSource,
                node.foreignSource,
                node.foreignId,
                node.location,
                node.createTime,
                node.parent ? node.parent.id : undefined,
                node.parent ? node.parent.foreignSource : undefined,
                node.parent ? node.parent.foreignId : undefined,
                node.type ? node.type.toDisplayString() : undefined,
                node.sysObjectId,
                node.sysName,
                node.sysDescription,
                node.sysLocation,
                node.sysContact,
                node.netBiosName,
                node.netBiosDomain,
                node.operatingSystem,
                node.lastCapsdPoll,
                primarySnmp && primarySnmp.physAddr ? primarySnmp.physAddr.toString() : undefined,
                primarySnmp ? primarySnmp.ifIndex : undefined,
                primaryIpInterface && primaryIpInterface.ipAddress ? primaryIpInterface.ipAddress.correctForm() : undefined,
                node.categories ? node.categories.map(cat => cat.name) : undefined,

                // Data Source
                self.name
            ];

            /*
            // Index the event parameters by name
            let eventParametersByName = {};
            if (node.lastEvent && node.lastEvent.parameters) {
              _.each(node.lastEvent.parameters, parameter => {
                eventParametersByName[parameter.name] = parameter.value;
              });
            }

            // Append the event parameters to the row
            row = row.concat(_.map(parameterNames, parameterName => {
              if (_.has(eventParametersByName, parameterName)) {
                return eventParametersByName[parameterName];
              } else {
                return undefined;
              }
            }));
            */

            row.meta = {
                // Store the alarm for easy access by the panels - may not be necessary
                'node': node,
                // Store the name of the data-source as part of the data so that
                // the panel can grab an instance of the DS to perform actions
                // on the alarms
                "source": this.name,
            };
            return row;
        });

        return [
            {
                "columns": columns,
                "rows": rows,
                "type": "table",
            }
        ];
    }

    getNode(nodeId) {
        return this.client.getNode(nodeId);
    }
}
