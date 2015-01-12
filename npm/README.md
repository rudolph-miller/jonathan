# Jonathan

React Mixins for Jonathan Simple JSON API Server

## Usage

```
var React = require("react");
var jonathan = require("react-jonathan");
var Api = jonathan.Api;

var Sample = React.createClass({
    mixins: [ Api ],
  
    getInitialState: function () {
        return {
            content: null
        };
    },
  
    api: "/api/sample",
  
    onLoad: function (json) {
        this.setState({content: json.content});
    },
  
    render: function () {
        var content;
        if (this.state.content) {
            content = (
                  <div>
                      You received content: {this.state.content}
                  </div>
              );
        }
    
        return (
            <div>
                {content}
            </div>
        );
    }
});

React.render(<Sample />, document.body);
```

## Install
`npm install --save react-jonathan`
