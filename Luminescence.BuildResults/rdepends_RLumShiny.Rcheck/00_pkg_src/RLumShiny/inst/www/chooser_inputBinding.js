/* 'chooser-binding.js' taken from the shiny-examples repository (https://github.com/rstudio/shiny-examples) under the MIT License

Modified by: sebastian.kreutzer@u-bordeaux-montaigne.fr
Date: 2017-07-28
Modification:
    > http://api.jquery.com/clone/ states:
    > As shown in the discussion for .append(), normally when an element is inserted somewhere in the DOM,
    > it is moved from its old location.

    However, this is not what we want ... not really, thus we modify the functions and now we
    have a clone() for left to right and a remove() for right to left.

*/

(function() {

  function updateChooser(chooser) {
    chooser = $(chooser);
    var left = chooser.find("select.left");
    var right = chooser.find("select.right");
    var leftArrow = chooser.find(".left-arrow");
    var rightArrow = chooser.find(".right-arrow");

    var canMoveTo = (left.val() || []).length > 0;  //returns only true or false
    var canMoveFrom = (right.val() || []).length > 0; //returns only true or false

    //this mutes the arrow, if nothing is left on one or the other side
    leftArrow.toggleClass("muted", !canMoveFrom);
    rightArrow.toggleClass("muted", !canMoveTo);
  }


  function remove(chooser, source, dest) {
    chooser = $(chooser);
    var selected = chooser.find(source).children("option:selected");
    selected.remove();
    updateChooser(chooser);
    chooser.trigger("change");
  }


  function copy(chooser, source, dest) {
    chooser = $(chooser);
    var selected = chooser.find(source).children("option:selected");
    var dest = chooser.find(dest);
    dest.children("option:selected").each(function(i, e) {e.selected = false;});
    selected.clone().appendTo(dest);
    updateChooser(chooser);
    chooser.trigger("change");
  }

  $(document).on("change", ".chooser select", function() {
    updateChooser($(this).parents(".chooser"));
  });

  $(document).on("click", ".chooser .right-arrow", function() {
    copy($(this).parents(".chooser"), ".left", ".right");
  });

  $(document).on("click", ".chooser .left-arrow", function() {
    remove($(this).parents(".chooser"), ".right", ".left");
  });

  $(document).on("dblclick", ".chooser select.left", function() {
    copy($(this).parents(".chooser"), ".left", ".right");
  });

  $(document).on("dblclick", ".chooser select.right", function() {
    remove($(this).parents(".chooser"), ".right", ".left");
  });

  var binding = new Shiny.InputBinding();

  binding.find = function(scope) {
    return $(scope).find(".chooser");
  };

  binding.initialize = function(el) {
    updateChooser(el);
  };

  binding.getValue = function(el) {
    return {
      left: $.makeArray($(el).find("select.left option").map(function(i, e) { return e.value; })),
      right: $.makeArray($(el).find("select.right option").map(function(i, e) { return e.value; }))
    }
  };

  binding.setValue = function(el, value) {
    // TODO: implement
  };

  binding.subscribe = function(el, callback) {
    $(el).on("change.chooserBinding", function(e) {
      callback();
    });
  };

  binding.unsubscribe = function(el) {
    $(el).off(".chooserBinding");
  };

  binding.getType = function() {
    return "shinyjsexamples.chooser";
  };

  Shiny.inputBindings.register(binding, "shinyjsexamples.chooser");

})();
