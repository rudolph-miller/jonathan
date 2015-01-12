/** @jsx React.DOM */

'use strict';

var React = require("react");
var qwest = require("qwest");

var Api = {
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

exports.Api = Api;
