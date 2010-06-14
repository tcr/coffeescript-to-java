(function(){
  var CoffeeScript, fs, helpers, no_such_task, oparse, options, optparse, path, print_tasks, switches, tasks;
  var __hasProp = Object.prototype.hasOwnProperty;
  // `cake` is a simplified version of [Make](http://www.gnu.org/software/make/)
  // ([Rake](http://rake.rubyforge.org/), [Jake](http://github.com/280north/jake))
  // for CoffeeScript. You define tasks with names and descriptions in a Cakefile,
  // and can call them from the command line, or invoke them from other tasks.
  // Running `cake` with no arguments will print out a list of all the tasks in the
  // current directory's Cakefile.
  // External dependencies.
  fs = require('fs');
  path = require('path');
  helpers = require('./helpers').helpers;
  optparse = require('./optparse');
  CoffeeScript = require('./coffee-script');
  // Keep track of the list of defined tasks, the accepted options, and so on.
  tasks = {};
  options = {};
  switches = [];
  oparse = null;
  // Mixin the top-level Cake functions for Cakefiles to use directly.
  helpers.extend(global, {
    // Define a Cake task with a short name, an optional sentence description,
    // and the function to run as the action itself.
    task: function(name, description, action) {
      var _a;
      if (!(action)) {
        _a = [description, action];
        action = _a[0];
        description = _a[1];
      }
      tasks[name] = {
        name: name,
        description: description,
        action: action
      };
      return tasks[name];
    },
    // Define an option that the Cakefile accepts. The parsed options hash,
    // containing all of the command-line options passed, will be made available
    // as the first argument to the action.
    option: function(letter, flag, description) {
      return switches.push([letter, flag, description]);
    },
    // Invoke another task in the current Cakefile.
    invoke: function(name) {
      if (!(tasks[name])) {
        no_such_task(name);
      }
      return tasks[name].action(options);
    }
  });
  // Run `cake`. Executes all of the tasks you pass, in order. Note that Node's
  // asynchrony may cause tasks to execute in a different order than you'd expect.
  // If no tasks are passed, print the help screen.
  exports.run = function() {
    return path.exists('Cakefile', function(exists) {
      var _a, _b, _c, _d, arg, args;
      if (!(exists)) {
        throw new Error(("Cakefile not found in " + (process.cwd())));
      }
      args = process.argv.slice(2, process.argv.length);
      CoffeeScript.run(fs.readFileSync('Cakefile').toString(), {
        source: 'Cakefile'
      });
      oparse = new optparse.OptionParser(switches);
      if (!(args.length)) {
        return print_tasks();
      }
      options = oparse.parse(args);
      _a = []; _c = options.arguments;
      for (_b = 0, _d = _c.length; _b < _d; _b++) {
        arg = _c[_b];
        _a.push(invoke(arg));
      }
      return _a;
    });
  };
  // Display the list of Cake tasks in a format similar to `rake -T`
  print_tasks = function() {
    var _a, _b, _c, _d, desc, i, name, spaces, task;
    puts('');
    _a = tasks;
    for (name in _a) { if (__hasProp.call(_a, name)) {
      task = _a[name];
      spaces = 20 - name.length;
      spaces = spaces > 0 ? (function() {
        _b = []; _c = 0; _d = spaces;
        for (i = _c; (_c <= _d ? i <= _d : i >= _d); (_c <= _d ? i += 1 : i -= 1)) {
          _b.push(' ');
        }
        return _b;
      })().join('') : '';
      desc = task.description ? ("# " + task.description) : '';
      puts(("cake " + name + spaces + " " + desc));
    }}
    if (switches.length) {
      return puts(oparse.help());
    }
  };
  // Print an error and exit when attempting to all an undefined task.
  no_such_task = function(task) {
    puts(("No such task: \"" + task + "\""));
    return process.exit(1);
  };
})();
