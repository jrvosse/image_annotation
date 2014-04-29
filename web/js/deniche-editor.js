annotorious.plugin.DenichePlugin.prototype._extendEditor = function(annotator) {
	var oSelf = this;
	var fieldsEl = document.getElementById('fields');

	this.saveButton   = document.getElementsByClassName('annotorious-editor-button-save').item(0); 
	this.cancelButton = document.getElementsByClassName('annotorious-editor-button-cancel').item(0); 

	annotator.editor.addField(fieldsEl);
	this.saveButton.innerHTML = "Done";

	anno.addHandler('onSelectionCompleted', function(ev) {
		// HACK:
		// set shape when new selection is completed 
		// so we can use it in the YUI autocompletion plugins.
		anno.currentShape = ev.shape;

	});

	anno.addHandler('onEditorShown', function(annotation) {
		// HACK:
		// set shape when adding to existing annotation 
		// so we can use it in the YUI autocompletion plugins.
		if (annotation && annotation.shapes) {
			anno.currentShape = annotation.shapes[0];
			oSelf.cancelButton.style.display="none";
			oSelf.saveButton.style.display="inline-block";
		} else {
			oSelf.saveButton.style.display="none";
			oSelf.cancelButton.style.display="inline-block";
		}

		YUI().use('node', 'event', function(Y) {
			// Y.one('a.annotorious-editor-button-save').setHTML('Done'); // Hack: turn save into done button
			Y.all('li.tagitem').each(function(tagNode) {
				if (annotation && annotation.targetId == tagNode.getAttribute('targetId'))
					tagNode.show();
				else
					tagNode.hide();

			})
		})
	});
}


