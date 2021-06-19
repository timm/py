# Exercises

1. Don't use KEYS0 to improve some  space. Use something else. 
   - See  how much
   better you can do  (measured in  runtime,  minimum  number of  y-value queries,
   fewest ranges presented to the  user, smallest  generated model, greate effect of the
   model.
   - Try  fully numeric methods
   - Try sequential model optimization  (TPE)
2. Write a good Github rego  (your  repo is your resume):
    - Register it at zenodo.  Make a release back on Gihub.
      Add your Zendo badge to your site readme.
    - Add some style  checker, style repair agent to your  editor.
      (e.g. for  Python)
    - Add a test engine  to your  code  that
      - Sets  `$?` if there is any  error;
      - Lets you  run all  your tests;
      - Lets you run one  specific test
    - Add a .travis.yml file so travis-ci.com runs tests whenever you
      commit to your code.
    - Write some documentation generator which, for your code (e.g. pydoc3,
      pycoo, etc, autogenerate  documentation  in your
      docs/ folder. 
3. Basic  Python
   - Write a regular regular expression that matches an  integer and,
     if matched, converts a string to  an int.
   - Write a regular regular expression that matches a  float and,
     if matched, converts a string to  a float.
   - Write a class that wraps a dictionary such that we can get
     and set fields  using `d.key` and `d.key=1`
   - Write  a class that turns  "public" instance variables into
     a  dictionary  of `var=value` pairs, the prints that dictionary
     using  `str(d)`. Install that as the `\_\_repr\_\_` method.
   - Given a dictionary `d` of key,value pairs,
     write  a small tool (less than 20 lines)
     that looks at the command like  (via sys.argv)
     looking for `-key value` 
     and which then  updates `d[key]` with `value`. 
   - Write a global storing defaults for t
   - Write a class `Eg` with class  methods  all, one, eg1,eg2.
     - Eg.all() gets a list  of all methods that start 
       with `eg`. ./keys -do X` runs `Eg.one(Eg.egX)`.
   - `Eg.one(fun)` that resets  the random
     number  seed (to some default, e.g. 10013), before it calls Eg.fun()

Write a  col factor that...
