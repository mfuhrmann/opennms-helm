import { FilterColumn } from '../../lib/filter_column';

import {entityTypes} from '../../datasources/entity-ds/datasource';

export class FilterPanelEditorCtrl {
  /** @ngInject */
  constructor($scope, $q, uiSegmentSrv, datasourceSrv) {
    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;
    this.datasourceSrv = datasourceSrv;
    $scope.editor = this;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    $scope.panel = this.panel;

    $scope.datasourceType = undefined;
    this.entityTypes = entityTypes;
    
    this.srcIndex = undefined;
    this.destIndex = undefined;

    this.addColumnSegment = uiSegmentSrv.newPlusButton();
    let editor = document.querySelectorAll('.editor-row')[0];
    for (const e of [ 'dragstart', 'dragover', 'dragleave', 'drop']) {
      //console.log('adding listener: ' + e);
      editor.addEventListener(e, (evt) => { this.handleEvent(e, evt); }, false);
    }

    if (!$scope.entityType) {
      $scope.entityType = entityTypes[0];
    }

    $scope.reset = this.reset;
    this.reset();
  }

  setDatasource(datasource) {
    this.datasourceSrv.get(datasource).then((ds) => {
      this.$scope.datasourceType = ds.type;
      console.log('Setting datasource to:', ds);
      console.log('Scope:', this.$scope);
    }).catch(() => {
      this.$scope.datasourceType = undefined;
    });
  }

  setEntityType(type) {
    const t = type ? type : this.entityTypes[0];
    this.$scope.entityType = type;
    console.log('Setting entity type to:', t);
    console.log('Scope:', this.$scope);
  }

  removeClasses(...classes) {
    for (const c of classes) {
      const cols = document.querySelectorAll('.' + c);
      [].forEach.call(cols, (col) => {
        col.classList.remove(c);
      });
    }
  }

  getTarget(evt) {
    if (evt.srcElement && evt.srcElement.offsetParent) {
      let target = evt.srcElement.offsetParent;
      if (target && target.id && target.classList && target.classList.contains('column-reorder')) {
        return target;
      }
    }
    // dragleave is only fired for the label and not the parent container
    if (evt.target && evt.target.parent && evt.target.parent.classList && evt.target.parent.classList.contains('column-reorder')) {
      return evt.target.parent;
    }
  }

  handleEvent(type, evt) {
    const target = this.getTarget(evt);
    const id = evt.srcElement.id;

    switch(type) {
      case 'dragstart':
        evt.srcElement.classList.add('picked-up');
        evt.dataTransfer.effectAllowed = 'move';
        // Internet Explorer doesn't support "text/html":
        // https://stackoverflow.com/a/28740710
        try {
          evt.dataTransfer.setData('text/html', evt.srcElement.innerHTML);
        } catch (error) {
          evt.dataTransfer.setData('text', evt.srcElement.innerHTML);
        }
        if (id) {
          this.srcIndex = parseInt(id.replace(/^column-/, ''), 10);
          console.log('picking up "' + this.panel.columns[this.srcIndex].text + '"');
        }
        break;
      case 'dragover':
        if (evt.preventDefault) {
          evt.preventDefault();
        }
        evt.dataTransfer.dropEffect = 'move';
        if (target && target.id && target.classList && target.classList.contains('column-reorder')) {
          const columnIndex = parseInt(target.id.replace(/^column-/, ''), 10);
          if (!target.classList.contains('over')) {
            //console.log('entering ' + this.panel.columns[columnIndex].text);
            this.removeClasses('over');
            target.classList.add('over');
            this.destIndex = columnIndex;
          }
        }
        break;
      case 'dragleave':
        if (target && evt.screenX !== 0 && evt.screenY !== 0) {
          //const columnIndex = parseInt(target.id.replace(/^column-/, ''), 10);
          //console.log('leaving ' + this.panel.columns[columnIndex].text);
          this.destIndex = undefined;
          this.removeClasses('over');
        }
        break;
      case 'drop':
        if (eval.stopPropagation) {
          evt.stopPropagation();
        }
        if (this.srcIndex !== undefined && this.destIndex !== undefined) {
          this.$scope.$apply(() => {
            this.panel.columns.splice(this.destIndex, 0, this.panel.columns.splice(this.srcIndex, 1)[0]);
            this.panelCtrl.render();
          });
          console.log('dropped "' + this.panel.columns[this.srcIndex].text + '" onto "' + this.panel.columns[this.destIndex].text + '"');
        } else {
          const targetIndex = (this.srcIndex == undefined) ? 'source' : 'destination';
          console.log(`WARNING: drop event received but ${targetIndex} was unset.`);
        }
        this.removeClasses('over', 'picked-up');
        return false;
      default:
        console.log('WARNING: unhandled event type: ' + type);
    }
  }

  async getColumnOptions() {
    const self = this;
    const $scope = self.$scope;
    const dsName = $scope.datasource;

    const ds = await self.datasourceSrv.get(dsName);

    await $scope.$evalAsync(() => {});

    const entityType = ds.type === 'opennms-helm-entity-datasource' ? $scope.entityType : undefined;

    const opts = {
      queryType: 'attributes'
    };
    if (entityType) {
      opts.entityType = entityType.id;
    }

    const res = await ds.metricFindQuery(entityType ? entityType.queryFunction + '()' : null, opts);

    // filter out columns that have already been selected
    const filtered = res.filter(a => self.panel.columns.indexOf(a) < 0);
    console.log('filtered:', filtered);

    const segments = filtered.map(c => this.uiSegmentSrv.newSegment({
      // datasource: ds.name,
      value: c.name
    }));

    return segments;
  }

  async addColumn() {
    const self = this;
    const $scope = self.$scope;

    const label = self.addColumnSegment.value;
    const dsName = $scope.datasource;

    const ds = await self.datasourceSrv.get(dsName);

    const entityType = ds.type === 'opennms-helm-entity-datasource' ? $scope.entityType : undefined;

    const opts = {
      queryType: 'attributes'
    };
    if (entityType) {
      opts.entityType = entityType.id;
    }

    const res = await ds.metricFindQuery(entityType ? entityType.queryFunction + '()' : null, opts);
    const match = res.filter(col => col.name === label)[0];
    if (match) {
      const label = match.name;
      const column = new FilterColumn(label, dsName, match.id, 'multi', entityType);
      console.log('adding column:', column);
      self.panel.columns.push(column);
    }

    const plusButton = self.uiSegmentSrv.newPlusButton();
    self.addColumnSegment.html = plusButton.html;
    self.addColumnSegment.value = plusButton.value;
    self.render();
  }

  render() {
    this.panelCtrl.render();
  }

  reset() {
    this.$scope.datasources = this.datasourceSrv.getMetricSources().filter(ds => {
      return !ds.meta.mixed && ds.value !== null;
    });
    this.$scope.datasource = this.panel.datasource;
    this.render();
  }

  removeColumn(column, index) {
    console.log('removing column:', column);
    this.panel.columns.splice(index, 1);
    this.reset();
  }
}

/** @ngInject */
export function filterPanelEditor($q, uiSegmentSrv) { // eslint-disable-line no-unused-vars
  'use strict';
  return {
    restrict: 'E',
    scope: true,
    templateUrl: '/public/plugins/opennms-helm-app/panels/filter-panel/editor.html',
    controller: FilterPanelEditorCtrl,
  };
}
