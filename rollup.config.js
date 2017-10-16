import nodeResolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import uglify from 'rollup-plugin-uglify';

const plugins = [
  nodeResolve(),
  commonjs({
    ignoreGlobal: true,
  }),
  uglify()
];

export default {
  input: './index.js',
  name: 'SweetsourParser',
  exports: 'named',
  output: [{
    file: 'dist/sweetsour-parser.min.js',
    format: 'umd'
  }],
  plugins
};
