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
    input: './index.js',
    name: 'SweetsourParser',
    exports: 'named',
    output: [{
      file: 'dist/sweetsour-parser.min.js',
      format: 'umd'
    }],
    plugins: plugins.concat(prodPlugins)
  }, {
    input: './index.js',
    name: 'SweetsourParser',
    exports: 'named',
    output: [{
      file: 'dist/sweetsour-parser.es.js',
      format: 'es'
    }],
    plugins
  }, {
    input: './index.js',
    name: 'SweetsourParser',
    exports: 'named',
    output: [{
      file: 'dist/sweetsour-parser.cjs.js',
      format: 'umd'
    }],
    plugins
  }
];
