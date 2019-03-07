import {MetricsPanelCtrl} from "app/plugins/sdk";
import {filterPanelEditor} from './editor';
import _ from "lodash";

const columnCache = {};

class FilterCtrl extends MetricsPanelCtrl {

    constructor($scope, $injector, datasourceSrv, templateSrv, variableSrv, timeSrv) {
        super($scope, $injector);

        this.datasourceSrv = datasourceSrv;
        this.templateSrv = templateSrv;
        this.variableSrv = variableSrv;
        this.timeSrv = timeSrv;

        _.defaults(this.panel, {
            columns: []
        });
        this.columnData = {};

        this.$scope.dashboard = this.dashboard;
        this.$scope.ctrl = this;

        this.events.on('init-edit-mode', this.onInitEditMode.bind(this));
        this.events.on('data-received', this.onDataReceived.bind(this));
        this.events.on('data-error', this.onDataError.bind(this));
        this.events.on('data-snapshot-load', this.onDataReceived.bind(this));
        this.events.on('render', this.onRender.bind(this));
    }

    link($scope, elem, attrs, ctrl) {
        // this.elem = elem.find('.histogram-chart');
        this.ctrl = ctrl;
        this.$scope = $scope;
    }

    onInitEditMode() {
        this.addEditorTab('Filtering', filterPanelEditor, 2);
    }

    onDataReceived(data) {
        this.columnData = data ? data[0] : {};
        this.$scope.columns = this.panel.columns.map(column => this.getColumn(column));
        this.$scope.columnVariables = this.$scope.columns.map(column => this.getVariable(column));

        //console.log('got data:', this.columnData);
        this.render();
    }

    onDataError() {
        this.columnData = {};
        /** reset data here */
        this.render();
    }

    onRender() {
        console.log('onRender: this=', this);
    }

    getColumn(obj) {
        if (this.columnData && this.columnData.columns) {
            const match = this.columnData.columns.filter(col => col.text === obj.text)[0];
            if (match) {
                Object.assign(obj, match);
            }
        }
        obj.type = 'query';
        console.log('getColumn:', obj);
        return obj;
    }

    getVariable(column) {
        const query = columnCache[column.text] ? columnCache[column.text] : this.variableSrv.createVariableFromModel(column);

        query.datasource = this.datasource.name;
        query.includeAll = true;
        query.label = column.text;
        query.resource = column.resource;
        query.query = column.resource;

        query.updateOptions().then(function() {
            console.log('query updated:', query);
        });

        query.multi = true;

        /*
        if (!query.options || query.options.length === 0) {
            query.options = [{
                text: 'All',
                value: '$__all',
            }];
        }

        const selected = query.options ? query.options.filter(opt => opt.selected).map(opt => opt.text) : [];

        if (this.columnData && this.columnData.columns) {
            let options = [{
                text: 'All',
                value: '$__all',
                selected: selected.indexOf('All') >= 0 || selected.length === 0
            }];

            let index = -1;
            this.columnData.columns.forEach((col, i) => {
                if (col.text === column.text) {
                    index = i;
                    return false;
                }
            });
            if (index >= 0) {
                const data = this.columnData.rows.map(row => row[index]);
                console.log('Found column index for ' + column.text + ': ' + index, data);
                options = options.concat(data.map(row => {
                    const text = '' + row;
                    return {
                        text: text,
                        value: row,
                        selected: selected.indexOf(text) >= 0
                    }
                }));
            } else {
                console.log('No column index for ' + column.text + ' :(');
            }
            query.options = options;
        }
        query.current = query.options.filter(opt => opt.selected);

        const currentTexts = query.current.map(col => col.text);
        query.linkText = currentTexts.join(' + ');

        */
        return query;
    }

    /*
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
    */
}

FilterCtrl.templateUrl = 'panels/filter-panel/module.html';

export {
    FilterCtrl
};
