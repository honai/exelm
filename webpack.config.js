const path = require('path')

module.exports =(env, argv) => {
  const isDev = argv.mode === 'development'
  const config = {
    entry: './src/index.js',
    output: {
      filename: 'main.js',
      path: path.resolve(__dirname, 'dist')
    },
    module: {
      rules: [
        {
          test: /\.html$/,
          exclude: /node_modules/,
          loader: 'file-loader',
          options: {
            name: '[name].[ext]'
          }
        },
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: 'elm-webpack-loader',
          options: {
            debug: isDev,
            optimize: !isDev
          }
        }
      ]
    }
  }
  return config
}
