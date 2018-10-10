const { readFileSync } = require('fs');
const { join } = require('path');
const Benchmark = require('benchmark');
const { padl, padr } = require('./utils');

const Sweetsour = require('..');
const Stylis = require('stylis');

const testCss = readFileSync(join(__dirname, './test.css')).toString('utf8');
const suite = Benchmark.Suite('css parser');

const stylis = new Stylis({
  global: false,
  cascade: false,
  keyframe: false,
  prefix: false,
  compress: false,
  semicolon: true,
});

const options = {
  onError: e => {
    console.error(e.message);
    e.currentTarget.failure = e.error;
  }
};

suite
  .add('stylis transform', () => {
    stylis('', testCss);
  }, options)
  .add('sweetsour lexer', () => {
    Sweetsour.lex([testCss], []);
  }, options)

function logStart () {
  console.log(this.name);
  console.log('-----------------------------------------------');
}

function logResults (e) {
  const t = e.target;

  if (t.failure) {
    console.error(padl(10, t.name) + 'FAILED: ' + e.target.failure);
  } else {
    const result = padl(10, t.name)
      + padr(13, t.hz.toFixed(2) + ' op/s')
      + ' \xb1' + padr(7, t.stats.rme.toFixed(2) + '%')
      + padr(15, ' (' + t.stats.sample.length + ' samples)');

    console.log(result);
  }
}

function logComplete () {
  console.log('-----------------------------------------------');
}

suite
  .on('start', logStart)
  .on('cycle', logResults)
  .on('complete', logComplete)
  .run({ 'async': true });
