const path = require('path');
const pluginTester = require('babel-plugin-tester');
const plugin = require('../index');

pluginTester({
  pluginName: '@sweetsour/babel-plugin',
  plugin,
  fixtures: path.join(__dirname, '../__fixtures__')
});
