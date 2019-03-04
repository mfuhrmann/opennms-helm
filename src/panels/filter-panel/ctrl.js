import {MetricsPanelCtrl} from "app/plugins/sdk";
import _ from "lodash";

class FilterCtrl extends MetricsPanelCtrl {

    constructor($scope, $injector) {
        super($scope, $injector);

        _.defaults(this.panel, {
            /*
            groupProperty: 'acknowledged',
            direction: 'horizontal',
            */
        });

        this.events.on('init-edit-mode', this.onInitEditMode.bind(this));
        this.events.on('data-received', this.onDataReceived.bind(this));
        this.events.on('data-error', this.onDataError.bind(this));
        this.events.on('data-snapshot-load', this.onDataReceived.bind(this));
        this.events.on('render', this.onRender.bind(this));
    }

    link($scope, elem, attrs, ctrl) {
        this.elem = elem.find('.histogram-chart');
        this.ctrl = ctrl;
    }

    onInitEditMode() {
        this.addEditorTab('Filtering', 'public/plugins/opennms-helm-app/panels/filter-panel/editor.html', 2);
    }

    onDataReceived(data) {
        console.log('got data:', data);
        this.render();
    }

    onDataError() {
        /** reset data here */
        this.render();
    }

    onRender() {

    }

    query(data, column) {
        let result = [];

        for (let i = 0; i < data.length; i++) {
            const columnIndex = _.findIndex(data[i].columns, {text: column});
            const rows = data[i] && data[i].rows ? data[i].rows : [];
            for (let j = 0; j < rows.length; j++) {
                result.push(data[i].rows[j][columnIndex]);
            }
        }

        return result;
    }
}

FilterCtrl.templateUrl = 'panels/filter-panel/module.html';

export {
    FilterCtrl
};
