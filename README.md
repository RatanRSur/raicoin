### Development Highlights

* **Test First** – This project was written with a test-first approach. Write a failing test, write minimal code to make that test pass, and then do any desired refactorings because the tests provide confidence.
* **Git History Integrity** – All the commits pass tests. This means that I can confidently run commands that operate on many commits. Applications include performace testing every *n* commits to find regressions that creeped in and using git bisect to quickly hunt down a bug.
* **Functional Core, Imperative Shell** – Keep the parts on the "inside" of the program as pure and immutable as possible and only use impure, side-effecting procedures at the "boundaries".

