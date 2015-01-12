/** @jsx React.DOM */

'use strict';

var React = require("react");
var qwest = require("qwest");

var Ajax = {
    options: {
        retries: 0
    },

    getCsrfToken: function () {
        return document.getElementsByName('csrf-token')[0].content;
    },
    
    get: function (url, data) {
        return qwest.get(url, data, this.options);
    },

    post: function (url, data) {
        if ( data ) {
            data._csrf_token = this.getCsrfToken();
        } else {
            data = {
                _csrf_token: this.getCsrfToken()
            };
        }

        return qwest.post(url, data, this.options);
    },

    delete: function (url, data) {
        if ( data ) {
            data._csrf_token = this.getCsrfToken();
        } else {
            data = {
                _csrf_token: this.getCsrfToken()
            };
        }

        return qwest.delete(url, data, this.options);
    }
};

var Api = {
    mixins: [ Ajax ],
    
    componentDidMount: function () {
        if ( !(this.api || this.props.api) ) {
            console.warn("No API path specified");
            return false;
        }
        this.get(this.api || this.props.api, this.query || this.props.query).then(function (json) {
            this.onLoad(json);
        }.bind(this));
    }
};

exports.Ajax = Ajax;
exports.Api = Api;
