YUI.add('annotation', function(Y) {

	var Node = Y.Node;

	function Annotation(config) {
		Annotation.superclass.constructor.apply(this, arguments);
	}
	Annotation.NAME = "aclist"; // use same name as Y.Plugin.AutoComplete to inherit css
	Annotation.NS = "annotation";
	Annotation.ATTRS = {
		commentNode: {
			value: null
		},
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
		},
		allowTextSubmit: {
			value:true
		}
	};

	Annotation.LIST_CLASS = 'taglist';
	Annotation.LIST_TEMPLATE = '<ul class="'+Annotation.LIST_CLASS+'"></ul>';

	Y.extend(Annotation, Y.Plugin.AutoComplete, {

		initializer: function(args) {
			this.tags = new Y.Recordset({records:{}});
			this.tags.on("add", this._addTags, this);
			this.tags.on("remove", this._removeTags, this);

			this.tagList = Node.create(Annotation.LIST_TEMPLATE);

			var parentNode = this.DEF_PARENT_NODE;
			parentNode.append(this.tagList);
			this.infoNode = new Y.Overlay({}).render(parentNode);
			this.deleteNode = new Y.Overlay({}).render(parentNode);
			this.deleteNode.hide();
			head = "";
			body = "<div h3 class='annotate-comment delete-comment'>";
			body += "<h3>Opmerkingen (optioneel):</h3>";
			body += "<textarea class='annotate-comment-input delete-comment-input' />";
			foot  = "<button id='cancel-delete'>Annuleren</button>";
			foot += "<button id='confirm-delete'>Verwijderen</button>";
			this.deleteNode.set("headerContent", head);
			this.deleteNode.set("bodyContent",   body);
			this.deleteNode.set("footerContent", foot);
			this.deleteNode.set("centered", true);
			this.deleteNode.set("width", "33%");

			this.on("activeItemChange", this._onHover, this);
			this.on("hoveredItemChange", this._onHover, this);
			this.on("select", this._onItemSelect, this);
			Y.delegate("click", this._onTagRemoveClick, this.tagList, 'li .remove', this);
			this.get("inputNode").on("key", this._onTextSubmit, 'enter', this);

			var commentNode = this.get('commentNode');
			if (commentNode) {
			  commentNode = Y.one('#'+commentNode);
			  this.set('commentNode', commentNode);
			  commentNode.on("key", this._onTextSubmit, 'enter', this);
			}
			this.getTags();
			Y.Global.on("done", this._unload, this);
		},

		_unload : function(e) {
				  Y.log("done event!");
				  this._onTextSubmit({});
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
			var body = tag.getValue("body");
			var comment = tag.getValue("comment");
			var link = tag.getValue("display_link");
			html = '<div class="label">';
			if (link == '')
				html += label;
			else
				html += '<a href="'+link+'">'+label+'</a>';

			if (comment && comment != "") {
			  html += ' (' + comment +')';
			}
			html += '</div><div class="remove"><a href="javascript:{}">x</a></div>';
			return html;
		},

		_onTagRemoveClick : function(e) {
			var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode")),
			    tags = this.tags,
			    record = tags.getRecordByIndex(index),
			    annotation = record.getValue("annotation");
			    label = record.getValue("label");
			this.deleteNode.set("headerContent", "<h3 class='delete_dialog'>Verwijder: "+ label +"</h3>");
			this.deleteNode.show();
			Y.one('.delete-comment-input').on("key", this._onDelete, "enter", this, annotation, index);
			Y.one('#confirm-delete').on("click", this._onDelete, this, annotation, index);
			Y.one('#cancel-delete').on("click", this._onCancel, this, annotation, index);
		},

		_onCancel : function() {
				Y.one('.delete-comment-input').detach("key", this._onDelete, "enter");
				Y.one('#confirm-delete').detach("click", this._onDelete);
				Y.one('#cancel-delete').detach("click", this._onCancel);
			      	this.deleteNode.hide();
			     },

		_onDelete : function (e,annotation, index) {
			var comment = e.currentTarget.get("value");
			e.currentTarget.set("value", "");
			var tags = this.tags;
			Y.one('.delete-comment-input').detach("key", this._onDelete, "enter");
			Y.one('#confirm-delete').detach("click", this._onDelete);
			Y.one('#cancel-delete').detach("click", this._onCancel);
			this.deleteNode.hide();

			Y.log('remove annotation '+annotation+' with comment: '+comment);
			Y.io(this.get("store.remove"), {
				data:{
					annotation:annotation,
					comment:comment
				},
				on:{success: function(e) {
					       tags.remove(index) }
				}
			});
		},
		getTags : function() {
			    var target = this.get('target'),
				 field = this.get('field');
			    var oSelf = this;
			    Y.io(this.get("store.get"),
				 { data: {
					 target: target,
					 field:  field
					 },
				   on: {
				       success: function(e,o) {
						  var r = Y.JSON.parse(o.responseText);
						  if (r && r[field] && r[field].annotations) {
						    var data =  r[field].annotations;
						    oSelf.tags.add(data);
						  }
						}
				       }
				 }
				);
			  },
		_onHover : function(e) {
			var infoNode = this.infoNode,
				active = e.newVal,
				body = '';
			if(active && active.getData().result.raw.info) {
				Y.log(active.getData());
				var scope = active.getData().result.raw.info.scopeNotes[0];
				var defin = active.getData().result.raw.info.definitions[0];
				var alts =  active.getData().result.raw.info.altLabels;
				if (scope && scope.en) { body += "<div class='scope'>"+scope.en+"</div>"; }
				if (defin && defin.en) { body += "<div class='defin'>"+defin.en+"</div>"; }
				for (var i=0; i<alts.length; i++) {
					body += "<span class='altLabel'>" +alts[i] + "</span>" ;
				}
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
			var item = e.details[0].result.raw;
			var comm = this.getComment();
			if (item.uri && item.label) {
			  this.submitAnnotation({type:"uri", value:item.uri}, item.label, comm);
			} else {
			  this.submitAnnotation({type:"literal", value: item}, item, comm);
			}
		},
		_onTextSubmit : function(e) {
			if (e.preventDefault) e.preventDefault();
			if(!this.get("activeItem")) {
				var value = this.getTag();
				var comm = this.getComment();
				this.submitAnnotation({type:"literal", value:value}, value, comm);
			}
		},

		getTag: function() {
			      var value = this.get("inputNode").get("value");
			      this.get("inputNode").set("value", "");
			      return value?value:'';
		},

		getComment: function() {
			      var commentNode = this.get("commentNode");
			      if (!commentNode) return "";
			      var c = commentNode.get("value");
			      commentNode.set("value", "");
			      return c;
		},

		submitAnnotation : function(body, label, comment) {
		        if (!body.value) return;
			Y.log('add tag: '+body.value+' with label: '+label+ ', comment: ' + comment);

			var inputNode = this.get("inputNode");
			var tags = this.tags;

			Y.io(this.get("store.add"), {
				data:{
					target:this.get("target"),
					field:this.get("field"),
					body:Y.JSON.stringify(body),
					label:label,
					comment: comment
				},
				on:{success: function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					tags.add({body:body, label:label, annotation:r.annotation, comment:comment, display_link:r.display_link});
					inputNode.focus();
				    }
				}
			});
		}

	});

	Y.Plugin.Annotation = Annotation;

}, '0.0.1', { requires: [
	'node','event','event-custom', 'autocomplete','overlay','recordset','io-base','json','querystring-stringify-simple'
	]
});
