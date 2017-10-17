import nodeResolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import uglify from 'rollup-plugin-uglify';

const plugins = [
  nodeResolve(),
  commonjs({
    ignoreGlobal: true,
  })
];

const prodPlugins = [
  uglify({
    toplevel: true,
    compress: {
      passes: 2
    }
  })
];

export default [
  {
    input: './lib/es6/src/Main.js',
    name: 'SweetsourParser',
    exports: 'named',
    output: [{
      file: 'dist/sweetsour-parser.min.js',
      format: 'umd'
    }],
    plugins: plugins.concat(prodPlugins)
  }, {
    input: './lib/es6/src/Main.js',
    name: 'SweetsourParser',
    exports: 'named',
    output: [{
      file: 'dist/sweetsour-parser.es.js',
      format: 'es'
    }],
    plugins
  }, {
    input: './lib/es6/src/Main.js',
    name: 'SweetsourParser',
    exports: 'named',
    output: [{
      file: 'dist/sweetsour-parser.cjs.js',
      format: 'umd'
    }],
    plugins
  }
];
