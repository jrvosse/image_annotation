annotorious.plugin.DenichePlugin.prototype._extendEditor = function(annotator) {
	// console.log('_extendEditor');
	var self = this;
	var fieldsEl = document.getElementById('fields');
	this._annotator = annotator;
	annotator.editor.addField(fieldsEl);

	anno.addHandler('onSelectionCompleted', function(event) {
		anno.currentShape = event.shape;
		// console.log(event.shape);
		// console.log(self._annotator);
	});
}


