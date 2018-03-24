import { basename, dirname, join } from 'path';

import nodeResolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import babel from 'rollup-plugin-babel';
import uglify from 'rollup-plugin-uglify-es';
import es3 from 'rollup-plugin-es3';
import filesize from 'rollup-plugin-filesize';
import alias from 'rollup-plugin-alias';
import { rollup as lernaAliases } from 'lerna-alias';

const pkg = require(join(process.cwd(), './package.json'))

const plugins = [
  commonjs({
    ignoreGlobal: true,
  }),
  nodeResolve({
    modulesOnly: true,
    jsnext: true
  }),
  es3(),
  babel({
    babelrc: false,
    plugins: [
      require.resolve('babel-plugin-closure-elimination'),
      require.resolve('babel-plugin-minify-dead-code-elimination')
    ]
  })
];

const prodPlugins = [
  ...plugins,
  alias(lernaAliases()),
  uglify({
    toplevel: true,
    mangle: {
      toplevel: true
    },
    compress: {
      passes: 2
    }
  }),
  filesize()
];

const basePkgName = basename(pkg.name);
const pkgName = `Sweetsour${basePkgName[0].toUpperCase()}${basePkgName.slice(1)}`;

const withBase = x => Object.assign({}, x, {
  input: pkg.source,
  name: pkgName,
  exports: 'named',
  useStrict: false,
  pureExternalImports: true
});

const umdDir = dirname(pkg['umd:main'])
const umdName = basename(pkg['umd:main'], '.js');

export default [
  {
    output: [{
      file: join(umdDir, `${umdName}.min.js`),
      format: 'umd'
    }],
    plugins: prodPlugins
  }, {
    output: [{
      file: pkg['umd:main'],
      format: 'umd'
    }, {
      file: pkg.module,
      format: 'es'
    }, {
      file: pkg.main,
      format: 'cjs'
    }],
    plugins
  }
].map(withBase);
