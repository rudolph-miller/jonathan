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
        var path = this.api || this.props.api;
        if ( !path ) {
            console.warn("No API path specified");
            return false;
        } else {
            path = "/api" + path;
            this.get(path, this.query || this.props.query).then(function (json) {
                this.onLoad(json);
            }.bind(this));
        }
    }
};

exports.Ajax = Ajax;
exports.Api = Api;
