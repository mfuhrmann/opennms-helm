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
            columns: [],
            selected: {},
            inputTypes: {}
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

        //console.log('got data:', this.columnData);
        this.render();
    }

    onDataError() {
        this.columnData = {};
        /** reset data here */
        this.render();
    }

    onRender() {
        this.$scope.columns = this.panel.columns.map(column => this.getColumn(column));
        this.$scope.columnVariables = this.$scope.columns.map(column => this.getVariable(column));
        console.log('onRender: this=', this);
    }

    typeChanged(col) {
        console.log('type changed:', col);
        if (col && col.text) {
            this.panel.inputTypes[col.text] = col.inputType;
        }
    }

    getColumn(obj) {
        if (this.columnData && this.columnData.columns) {
            const match = this.columnData.columns.filter(col => col.text === obj.text)[0];
            if (match) {
                Object.assign(obj, match);
            }
        }
        obj.type = 'query';
        obj.inputType = obj.inputType || this.panel.inputTypes[obj.text] || 'multi';
        console.log('getColumn:', obj);
        return obj;
    }

    getVariable(column) {
        if (!this.datasource) {
            // things are not quite initialized yet
            return undefined;
        }

        const label = column.text;
        const resource = column.resource;

        let query;
        if (columnCache[label]) {
            query = columnCache[label];
        } else {
            query = this.variableSrv.createVariableFromModel(column);
            const selected = this.panel.selected[label];
            console.log('query created:', query);
            console.log('selected:', selected);
            if (selected) {
                query.options.forEach(opt => {
                    opt.selected = selected.value.contains(opt.value);
                });
                query.current = selected;
                if (query.current) {
                    query.current.resource = resource;
                }
            }
        }

        query.datasource = this.datasource.name;
        query.includeAll = true;
        query.label = label;
        query.resource = resource;
        query.query = resource;

        query.updateOptions().then(function() {
            console.log('query updated:', query);
        });

        query.multi = column.inputType === 'multi' || column.inputType === undefined;
        return query;
    }

    variableUpdated(variable) {
        const self = this;

        console.log('variable updated:', variable);
        if (variable.current) {
            variable.current.resource = variable.resource;
        }
        this.panel.selected[variable.label] = variable.current;
        console.log('variable updated: panel:', self.panel);
        console.log('variable updated: self:', self);
        self.dashboard.panels.forEach(panel => {
            if (panel === self.panel) {
                console.log('skipping ' + panel.title);
            } else {
                console.log('re-rendering ' + panel.title);
                panel.refresh();
            }
        });
    }
}

FilterCtrl.templateUrl = 'panels/filter-panel/module.html';

export {
    FilterCtrl
};
