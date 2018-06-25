const path = require('path');
const istfUtils = require('@sweetsour/istf-utils');
const { parseTemplate, nodeStreamToOutput } = require('@sweetsour/parser');
const { hash } = require('./hash');

// Prefix string if it starts with a digit
const prefixLeadingDigit = str => str.replace(/^(\d)/, 'x$1');

module.exports = ({ types: t }) => {
  // does identNode match `styled`
  const isStyledIdentifier = identNode => (
    t.isIdentifier(identNode) &&
    identNode.name === 'styled'
  );

  // does tagNode match `styled.div`
  const isStyledMember = tagNode => (
    t.isMemberExpression(tagNode) &&
    isStyledIdentifier(tagNode.object)
  );

  // does tagNode match `styled(x)`
  const isStyledCall = tagNode => (
    t.isCallExpression(tagNode) &&
    isStyledIdentifier(tagNode.callee)
  );

  // does tagNode match `css`
  const isHelperCall = tagNode => (
    t.isIdentifier(tagNode) &&
    tagNode.name === 'css'
  );

  // is TaggedTemplateExpression node targetable
  const isStyledTaggedTemplate = node => {
    const tagNode = node.tag;
    return isStyledMember(tagNode)
      || isStyledCall(tagNode)
      || isHelperCall(tagNode);
  };

  // See: https://github.com/styled-components/babel-plugin-styled-components/blob/master/src/visitors/displayNameAndId.js#L40
  const getBlockName = file => {
    const { opts: { filename } } = file;
    const name = path.basename(filename, path.extname(filename));
    if (name === 'index') {
      return path.basename(path.dirname(filename));
    }

    return name;
  };

  // See: https://github.com/styled-components/babel-plugin-styled-components/blob/master/src/utils/getName.js
  const getPathName = path => {
    let namedNode

    path.find(path => {
      // const X = styled
      if (path.isAssignmentExpression()) {
        namedNode = path.node.left
        // const X = { Y: styled }
      } else if (path.isObjectProperty()) {
        namedNode = path.node.key
        // class Y { (static) X = styled }
      } else if (path.isClassProperty()) {
        namedNode = path.node.key
        // let X; X = styled
      } else if (path.isVariableDeclarator()) {
        namedNode = path.node.id
      } else if (path.isStatement()) {
        // we've hit a statement, we should stop crawling up
        return true
      }

      // we've got an displayName (if we need it) no need to continue
      if (namedNode) return true
    })

    // foo.bar -> bar
    if (t.isMemberExpression(namedNode)) {
      namedNode = namedNode.property
    }

    // identifiers are the only thing we can reliably get a name from
    return t.isIdentifier(namedNode) ? namedNode.name : undefined
  };

  // Approximate a display name for current path
  const getDisplayName = (path, state) => {
    const { file } = state
    const componentName = getPathName(path);
    if (!file) {
      return componentName;
    }

    const blockName = getBlockName(file);
    if (blockName === componentName) {
      return componentName;
    }

    const normBlockName = prefixLeadingDigit(blockName);
    if (componentName) {
      return `${normBlockName}__${componentName}`;
    }

    return normBlockName;
  };

  // Hashes the contents of nodes and outputs a hash string
  const hashNodes = nodes => {
    const str = nodes.reduce((acc, node) => {
      if (!istfUtils.isRef(node) && istfUtils.isEmpty(node)) {
        return acc + node[0] + node[1] + ';';
      }

      return acc + node[0];
    }, '');

    return hash(str);
  };

  // convert ISTF nodes to Babel types
  const convertNodesToTypes = nodes => {
    const nodeTypes = nodes.map(node => {
      // node[0] is the node's kind which is a number
      const nodeLiterals = [
        t.numericLiteral(node[0])
      ];

      // node[1] varies depending on the node's kind but is either:
      // - a string
      // - a ref (babel type)
      // - an identifier (number)
      if (istfUtils.isString(node)) {
        nodeLiterals.push(t.stringLiteral(node[1]));
      } else if (istfUtils.isRef(node)) {
        nodeLiterals.push(node[1]);
      } else if (istfUtils.isIdentifier(node)) {
        nodeLiterals.push(t.numericLiteral(node[1]));
      }

      // The output is still a tuple
      return t.arrayExpression(nodeLiterals);
    });

    return t.arrayExpression(nodeTypes);
  };

  // Assembles new tagged template argument
  const makeArgument = (name, hash, nodes) => t.objectExpression([
    t.objectProperty(t.stringLiteral('name'), t.stringLiteral(name)),
    t.objectProperty(t.stringLiteral('hash'), t.stringLiteral(hash)),
    t.objectProperty(t.stringLiteral('nodes'), nodes)
  ]);

  // Visitor for tagged template expressions
  const TaggedTemplateExpression = (path, state) => {
    const { node } = path;
    if (!isStyledTaggedTemplate(node)) {
      return;
    }

    // Extract strings and interpolations (expressions)
    const { tag: callee, quasi: { quasis, expressions }} = node
    const strings = quasis.map(quasi => quasi.value.cooked);

    // Parse tagged template expression as CSS using Sweetsour parser
    const istfNodes = nodeStreamToOutput(parseTemplate(strings, expressions));
    // Generate a hashed name
    const hashName = hashNodes(istfNodes);
    // Extract a displayName based on path and file
    const displayName = getDisplayName(path, state);
    // Convert nodes to babel types
    const istfNodeTypes = convertNodesToTypes(istfNodes);
    // Pass an object of a hash and the ISTF nodes
    const newArgument = makeArgument(displayName, hashName, istfNodeTypes);

    // Replace tagged template expression
    // The replacement is a call with the new argument
    path.replaceWith(t.callExpression(callee, [newArgument]));
  };

  return { visitor: { TaggedTemplateExpression } };
};
