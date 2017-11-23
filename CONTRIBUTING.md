Thanks for reading this guide and considering to contribute code to the project!
We always welcome contributions to Sweetsour.

This document will get you started on how to contribute and things you should know.
So please give it a thorough read.

This guide will help you to get started setting up the repository, and how to contribute
a change. Please read it carefully.

If you're reading this, please take a look at our [Code of Conduct](CODE_OF_CONDUCT.md)
as well.

## How do I contribute to the ISTF spec?

If you'd like to contribute to the ISTF spec instead, take a look at its repo: [cssinjs/istf-spec](https://github.com/cssinjs/istf-spec)

## How do I set up the project?

First run `yarn install` to install all dependencies.

The code is located inside `src/` and the tests are inside `__tests__/`.
To run the tests use `yarn test`, which runs Jest.

To build the Reason code run `yarn build`.

If you don't feel comfortable making changes to the Reason code, feel free to contribute
(open a PR) for a small change first, and ask for help. Alternatively reach out to one
of the contributors or open an issue instead.

## How do I contribute code?

1. Search for something you'd like to change. This can be an open issue, or just a feature
  you'd like to implement. Make sure that no one else is already on it, so that you're not
  duplicating someone else's effort.

2. Fork the repository and then clone it, i.e. `git clone https://github.com/YOUR_NAME/sweetsour-parser.git`

3. Checkout a new branch with a descriptive name, e.g. `git checkout -b fix/issue-123`

4. Make your changes :sparkles:

5. Update the tests if necessary and make sure that the existing ones are still passing using `yarn test`

6. Commit your changes with a short description, `git add -A && git commit -m 'Your meaningful and descriptive message'`

7. Push your new branch, `git push -u origin fix/issue-123`

8. Finally, open a pull request with a title and a short summary of what has been changed and why.

9. Wait for a maintainer to review it and make some changes as they're being recommended and as you see fit.

10. Get it merged and make cool a celebratory pose! :dancer:

