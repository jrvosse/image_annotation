YUI.add('annotation', function(Y) {

	var Node = Y.Node;
	
	function Annotation(config) {
		Annotation.superclass.constructor.apply(this, arguments);
	}
	Annotation.NAME = "aclist"; // use same name as Y.Plugin.AutoComplete to inherit css
	Annotation.NS = "annotation";  
	Annotation.ATTRS = {
		target: {
			value: null
		},
		field: {
			value: null
		},
		store: {
			value: null
		},
		tags: {
			value: []
		}
	};

	Annotation.LIST_CLASS = 'taglist';
	Annotation.LIST_TEMPLATE = '<ul class="'+Annotation.LIST_CLASS+'"></ul>';

	Y.extend(Annotation, Y.Plugin.AutoComplete, {

		initializer: function(args) {
			var parentNode = this.DEF_PARENT_NODE,
				tags = new Y.Recordset({records:this.get("tags")});
				
			this.tagList = parentNode.appendChild(Node.create(Annotation.LIST_TEMPLATE));
			this.infoNode = new Y.Overlay({
			}).render(parentNode);
			
			this._renderTags(tags._items, 0); // how to get the items nicely?
			tags.on("add", this._addTags, this);
			tags.on("remove", this._removeTags, this);
			this.on("hoveredItemChange", this._onHover, this);
			this.on("select", this._onItemSelect, this);
			Y.delegate("click", this._onTagRemoveClick, this.tagList, 'li .remove', this);
			this.tags = tags;
		},
	
		_renderTags : function(tags, index) {
			var tagList = this.tagList;
			// format the tags
			for(var i=0; i < tags.length; i++) {
				tagList.append('<li>'+this.formatTag(tags[i])+'</li>');	
			}
		},		
		_addTags : function(o) {
			this._renderTags(o.added, o.index);
		},
		_removeTags : function(o) {
			var tagNodes = this.tagList.all("li"),
				index = o.index,
				range = o.range;	
			for (var i=index; i < index+range; i++) {
				tagNodes.item(i).remove();
			}
		},
			
		formatTag : function(tag) { 
			var label = tag.getValue("label");
			html = '<div class="label">'+label+'</div>';				
 			html += '<div class="remove"><a href="javascript:{}">x</a></div>';
			return html;
		},
		
		_onTagRemoveClick : function(e) {
			var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode")),
				tags = this.tags,
				record = tags.getRecordByIndex(index),
				annotation = record.getValue("annotation");
			Y.log('remove annotation '+annotation+' at index: '+index);
			
			Y.io(this.get("store.remove"), {
				data:{
					annotation:annotation
				},
				on:{success: function(e) { tags.remove(index) }
				}
			});
		},
		_onHover : function(e) {
			var infoNode = this.infoNode,
				active = e.newVal,
				body = '';
			if(active) {	
				var scope = active.getData().result.raw.info.scopeNotes[0],
					defin = active.getData().result.raw.info.definitions[0];
				if (scope) { body += "<div class='scope'>"+scope+"</div>"; } 
				if (defin) { body += "<div class='defin'>"+defin+"</div>"; }
			}
			if(body) {
				infoNode.set("bodyContent", body);
				infoNode.set("align", {node:active, 
				                      points:[Y.WidgetPositionAlign.TL, Y.WidgetPositionAlign.TR]});
				infoNode.show();
			} else {
				infoNode.hide();
			}
		},
		_onItemSelect : function(e) {
			var item = e.details[0].result.raw,
				uri = item.uri,
				label = item.label,
				inputNode = this.get("inputNode");
				
			Y.log('add tag: '+label+' '+uri);
			var tags = this.tags;
			Y.io(this.get("store.add"), {
				data:{
					target:this.get("target"),
					field:this.get("field"),
					body:uri,
					label:label
				},
				on:{success: function(e,o) { 
					var r = Y.JSON.parse(o.responseText);
					tags.add({uri:uri, label:label, annotation:r.annotation});
					inputNode.set("value", "");
					}
				}
			});
			
		}
		
	});

	Y.Plugin.Annotation = Annotation;

}, '0.0.1', { requires: [
	'node','event','autocomplete','overlay','recordset','io-base','querystring-stringify-simple'
	]
});
