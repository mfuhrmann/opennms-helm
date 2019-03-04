import {OpenNMSInventoryDatasource} from './datasource';
import {OpenNMSInventoryDatasourceQueryCtrl} from './query_ctrl';
import {Examples} from './Examples';
import '../../components/timeout';

class OpenNMSInventoryDatasourceConfigCtrl {}
OpenNMSInventoryDatasourceConfigCtrl.templateUrl = 'datasources/inventory-ds/partials/config.html';

class OpenNMSInventoryDatasourceQueryOptionsCtrl {
  constructor(uiSegmentSrv) {
    this.uiSegmentSrv = uiSegmentSrv;
    this.examples = Examples;
  }

  createQueryFromExample(example) {
      const target = {
        isNew: true,
        filter: example.apiFilter
      };

      this.panelCtrl.panel.targets.push(target);
  }
}
OpenNMSInventoryDatasourceQueryOptionsCtrl.templateUrl = 'lib/query/partials/query.options.html';

export {
  OpenNMSInventoryDatasource as Datasource,
  OpenNMSInventoryDatasourceQueryCtrl as QueryCtrl,
  OpenNMSInventoryDatasourceConfigCtrl as ConfigCtrl,
  OpenNMSInventoryDatasourceQueryOptionsCtrl as QueryOptionsCtrl,
};
