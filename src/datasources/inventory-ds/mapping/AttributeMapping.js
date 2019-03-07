import _ from 'lodash';

export class AttributeMapping {
    constructor() {
        this.attributeMapping = {
            category: 'category.name',
            categories: 'category.name',
            'categories.name': 'category.name',
            ifIndex: 'snmpInterface.ifIndex',
            ipAddress: 'ipInterface.ipAddress',
            ipHostname: 'ipInterface.ipHostname',
            location: 'location.locationName',
            parentId: 'parent.id',
            parentForeignSource: 'parent.foreignSource',
            parentForeignId: 'parent.foreindId',
        };
    }

    getUiAttribute(externalAttribute) {
        const internalAttribute = _.findKey(this.attributeMapping, function(value) {
            return value === externalAttribute;
        });
        return internalAttribute || externalAttribute;
    }

    getApiAttribute(internalAttribute) {
        return this.attributeMapping[internalAttribute] || internalAttribute;
    }
}