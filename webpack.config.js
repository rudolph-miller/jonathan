var webpack = require('webpack');

module.exports = {
    entry: './react/index.jsx',
    output: {
        filename: 'static/js/bundle.js'
    },
    module: {
        loaders: [
            {
                test: /\.jsx$/,
                loader: 'jsx-loader?insertPragma=React.DOM&harmony'
            }
        ]
    },
    resolve: {
        extensions: ['', '.js', '.jsx']
    },
    resolveLoader: {
        modulesDirectories: [ "node_modules" ]
    }
}
