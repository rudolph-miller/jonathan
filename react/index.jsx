/** @jsx React.DOM */

'use strict';

var React = require("react");

var Sample = React.createClass({
    render: function () {
        return (
            <div>
                Sample App with Jonathan and React.js
            </div>
        );
    }
});

React.render(<Sample />, document.body);
