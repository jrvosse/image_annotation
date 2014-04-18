annotorious.plugin.denichePlugin = function(opt_config_options) { }
annotorious.plugin.denichePlugin.prototype.initPlugin = function(anno) { }

annotorious.plugin.denichePlugin.prototype.onInitAnnotator = function(annotator) {
  var fEl = document.getElementById('fields');
  annotator.editor.addField(fEl);
}

anno.addPlugin('denichePlugin', {});

anno.addHandler('onAnnotationCreated', function(annotation) {
	  console.log(annotation.text);
	  console.log(annotation);
	  console.log(annotation.shapes);
	  console.log(annotation.shapes[0]);
	  console.log(annotation.shapes[0].geometry);
});
