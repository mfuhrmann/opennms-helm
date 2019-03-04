import _ from 'lodash';

export class AttributeMapping {
    constructor() {
        this.attributeMapping = {
            'category': 'category.name',
            /*
            'location': 'location.locationName',
            'service': 'serviceType.name',
            'ipAddress': 'ipAddr',
            'severity': 'severity'
            */
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