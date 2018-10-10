const padl = (n, s) => {
  let res = s;
  while(res.length < n) {
    res += ' ';
  }

  return res;
};

const padr = (n, s) => {
  let res = s;
  while (res.length < n) {
    res = ' ' + res;
  }

  return res;
};

module.exports = { padl, padr };
