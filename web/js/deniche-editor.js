annotorious.plugin.DenichePlugin.prototype._extendEditor = function(annotator) {
	var oSelf = this;
	var fieldsEl = document.getElementById('fields');

	this.saveButton   = document.getElementsByClassName('annotorious-editor-button-save').item(0); 
	this.cancelButton = document.getElementsByClassName('annotorious-editor-button-cancel').item(0); 

	annotator.editor.addField(fieldsEl);
	this.saveButton.innerHTML = "Done";

	anno.addHandler('onSelectionCompleted', function(ev) {
		anno.currentShape = ev.shape;
	});

	anno.addHandler('onEditorShown', function(annotation) {
		if (annotation && annotation.shapes) {
			anno.currentShape = annotation.shapes[0];
			oSelf.toggleButtons('done', annotation);
			oSelf.filterTags(annotation);
		} else {
			oSelf.toggleButtons('new', annotation);
			oSelf.filterTags(null);
		}
	});
}


