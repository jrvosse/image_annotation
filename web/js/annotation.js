/*
 * Annotation class represents a single annotation field.
 * More complex interfaces can be built by combining multiple fields.
 *
 */

YUI.add('annotation', function(Y) {
	function Annotation(config) {
		Annotation.superclass.constructor.apply(this, arguments);
	}
	Annotation.NAME = "aclist"; // use same name as Y.Plugin.AutoComplete to inherit css
	Annotation.NS = "annotation";
	Annotation.ATTRS = {
		target:			{ value: null }, // URI of target image to be annotated
		field:			{ value: null }, // URI identifying annotation field
		store:			{ value: null }, // URIs of web services to CRUD http annotation api
		startTyping:		{ value: null }, // timestamp when users start typing
		myMetaTags:		{ value: {} },   // myMetaTags dictionary
		uiLabels:		{ value: [] },   // dictionary with ui labels in the prefered language of the user
		next:			{ value: null }, // id of next field to tab to
		user:                   { value: "anonymous" },

		// configuration options:
	        deleteEnabled:          { value: "mine" },   // when delete icon is shown each tag
		commentEnabled:		{ value: "always" }, // when comment icon is shown for each tag
		unsureEnabled:		{ value: "always" }, // when "I'm not sure" checkboxes will be shown for each tag
		agreeEnabled:		{ value: "yours" },  // when "I agree" checkboxes will be shown for each tag
		disagreeEnabled:	{ value: "yours" },  // when "I disagree" checkboxes will be shown for each tag
		deleteCommentEnabled:	{ value: "always" }, // when comment overlay is shown for deletions on this field
	};

	Annotation.LIST_CLASS = 'taglist';
	Annotation.LIST_TEMPLATE = '<ul class="'+Annotation.LIST_CLASS+'"></ul>';

	Y.extend(Annotation, Y.Plugin.AutoComplete, {

		initializer: function(args) {
			var next = this.get("next");
			var parentNode = this.DEF_PARENT_NODE;

			// handler to call when item selected from autocompletion suggestions:
			this.on("select", this.onItemSelect, this);

			// handler to call when hitting return after string input (no autocomplete):
			this.get("inputNode").on("key", this.onTextSubmit, "enter", this, null);
			this.get("inputNode").on("key", this.onTextSubmit, "tab",   this, next);

			// create tags recordset (tag model in mvc), bind events to auto-update the tagList node (tag view in mvc):
			this.tags = new Y.Recordset({records:{}});
			this.tags.on("add", this.addTags, this);
			this.tags.on("update", this.updateTags, this);
			this.tags.on("remove", this.removeTags, this);

			// create tagList node (tag view in mvc)
			this.tagList = Y.Node.create(Annotation.LIST_TEMPLATE);
			parentNode.append(this.tagList);

			// infoNode is the overlay with tooltips when hovering over suggested terms
			this.infoNode = new Y.Overlay({}).render(parentNode);
			this.on("activeItemChange",  this.onSuggestionHover, this);
			this.on("hoveredItemChange", this.onSuggestionHover, this);

			// create overlays for comments on delete and add actions:
			this.createCommentNode(parentNode);
			if (this.enabled('deleteCommentEnabled', null)) {
				Y.delegate("click", this.onTagRemoveClick, this.tagList, 'li .remove', this);
				this.createDeleteNode(parentNode);
			} else {
				Y.delegate("click", this.onDelete, this.tagList, 'li .remove', this);
			}
			Y.delegate("click", this.onCommentRemoveClick, this.tagList, '.comment_remove a', this);


			// setup handlers to record typing time:
			var firstkey = true;
			this.setKeyInputHandler(firstkey);

			// everything is ready to request server for existing tags:
			this.getTags();
		},

		// handlers for adding additions, updates or removals in the tag Recordset:
		addTags : function(o) {
			    // Y.log('adding tags at index ' + o.index + ':');
			    // Y.log(o.added[0].getValue());
			    this.renderTags(o.added, o.index);
			  } ,
		updateTags : function(o) {
			    // Y.log('updating tags at index ' + o.index + ':');
			    // Y.log(o.updated[0].getValue());
			    this.renderTags(o.updated, o.index);
			     },
		removeTags : function(o) {
			var tagNodes = this.tagList.all("li");
			for (var i=o.index; i < o.index+o.range; i++) {
				var node = tagNodes.item(i);
				node.one('.label').detach('hover');
				node.remove();

			}
		},

		renderTags : function(tags, index) {
			var tagList = this.tagList;
			var tagNodes = this.tagList.all("li");
			if (index < tagNodes.size()) {
				this.removeTags({index:index, range:tags.length});
			}
			// format the tags
			for(var i=0; i < tags.length; i++) {
				var node = Y.Node.create('<li>'+this.formatTag(tags[i])+'</li>');
				tagList.insert(node, index+i);
				// Y.log('binding onTagHover on label ');
				// Y.log(node.one('.label'));
				node.one('.label').on('hover', this.onTagHover, this.onTagHover, this, tags[i]);
			};
		},

		enabled : function(option, tag) {
			var when   = this.get(option);
			var user   = this.get("user");
			var author = tag?tag.annotator:"no_user!";

			if (when == "always")
			  return true;
			else if (when == "never")
			  return false;
			else if (when == "mine")
			  return (user == author);
			else if (when == "yours")
			  return (user != author);
		},

		formatTag : function(tag) {
			var label = tag.getValue("title");
			var html = "";

			if (this.enabled('deleteEnabled', tag.getValue())) {
			  html += '<div class="remove"><a href="javascript:{}">x</a></div>';
			}
			html += "<div class='label'>" + label + "</div>";
			return html;
		},

		formatTagOverlay : function(tag) {
			var target= tag.hasTarget;
			var body  = tag.hasBody;
			var label = tag.title;
			var link  = tag.display_link;
			var annot = tag.annotation;
			var mymeta     = this.get('myMetaTags')[annot];
			var screenName = tag.screenName;

			var judgement_buttons = '';
			var my_rating = '';
			if (this.enabled('agreeEnabled', tag)) {
				var agreeLabel = this.get('uiLabels').agreeLabel;
				var agree_value = undefined;
				if (mymeta && mymeta.agree) agree_value = mymeta.agree.hasBody.value;
				var checked = 'unchecked';
				if (agree_value != undefined) {
				  checked = 'checked'; my_rating = agreeLabel;
				}
				judgement_buttons += "<span title='" + agreeLabel + "' ";
				judgement_buttons += "class='judgeButton agreeButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/thumbUp.png' title='" + agreeLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('unsureEnabled', tag)) {
				var unsureLabel = this.get('uiLabels').unsureLabel;
				var unsure_value = undefined;
				if (mymeta && mymeta.unsure) unsure_value = mymeta.unsure.hasBody.value;
				var checked = 'unchecked';
				if (unsure_value != undefined){
				  checked = 'checked'; my_rating = unsureLabel
				}
				judgement_buttons += "<span title='" + unsureLabel + "' ";
				judgement_buttons += "class='judgeButton unsureButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/unsure.png' title='" + unsureLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('disagreeEnabled', tag)) {
				var disagreeLabel = this.get('uiLabels').disagreeLabel;
				var disagree_value = undefined;
				if (mymeta && mymeta.disagree) disagree_value = mymeta.disagree.hasBody.value;
				var checked = 'unchecked';
				if (disagree_value != undefined){
				  checked = 'checked'; my_rating = disagreeLabel
				}
				judgement_buttons += "<span title='" + disagreeLabel + "' ";
				judgement_buttons += "class='judgeButton disagreeButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/thumbDown.png' title='" + disagreeLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('commentEnabled', tag)) {
				var commentLabel = this.get('uiLabels').commentLabel;
				judgement_buttons += "<span title='" + commentLabel + "' ";
			        judgement_buttons += "class='judgeButton unchecked commentButton'>";
				judgement_buttons += "<img src='./icons/bubble.png' title='" + commentLabel + "'/>";
				judgement_buttons += "</span>";
			}

			var buttons = '<div class="commentButtons">' + judgement_buttons + '</div>';
			var html = '';
			html +=	'<div class="overlay label">';
			if (link == '')
				html += label;
			else
				html += '<a href="'+link+'">'+label+'</a>';
			html += '</div>'

			html += '<div class="overlay screenName">'+screenName+'</div>';

			if (mymeta && mymeta.comment) {
			  var comment = mymeta.comment;
			  html += '<div class="overlay comment">';
			  // html += '<span class="screenName">' + comment.screenName + "</span>";
			  html += '<span class="body">'       + comment.hasBody.value + "</span>";
			  if (this.enabled('deleteEnabled', tag)) {
			    html += '<div class="comment_remove"><a alt="' + comment.annotation + '">x</a></div>';
			  }
			  html += '</div>';
			}

			if (my_rating) {
			  html += '<div class="overlay rating">';
			  html += my_rating;
			  html += '</div>';
			}

			html += buttons;

			return html;
		},

		onTagHover: function(ev, tag) {
			var overlay = tag.getValue('overlay');
			if (ev.phase == 'over') {
			  overlay.render(ev.target);
			  overlay.set('width','25em');
			  overlay.set("align", {node:ev.target,
			                        points:[Y.WidgetPositionAlign.RC, Y.WidgetPositionAlign.TL]});

			  overlay.show();
			  var node = overlay.get('srcNode');
			  node.all('.judgeButton').detach('click');

			  node.one('.commentButton').on(           'click', this.onCommentAnnotation, this, tag);

			  node.all('.unsureButton.unchecked').on(  'click', this.onJudgeAnnotation, this, 'add', 'unsure',   tag);
			  node.all('.agreeButton.unchecked').on(   'click', this.onJudgeAnnotation, this, 'add', 'agree',    tag);
			  node.all('.disagreeButton.unchecked').on('click', this.onJudgeAnnotation, this, 'add', 'disagree', tag);

			  node.all('.unsureButton.checked').on(    'click', this.onJudgeAnnotation, this, 'rm',  'unsure',   tag);
			  node.all('.agreeButton.checked').on(     'click', this.onJudgeAnnotation, this, 'rm',  'agree',    tag);
			  node.all('.disagreeButton.checked').on(  'click', this.onJudgeAnnotation, this, 'rm', ' disagree', tag);
			} else {
			  overlay.hide();
			}
		},

		onTagRemoveClick : function(e) {
			var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode")),
			    tags = this.tags,
			    record = tags.getRecordByIndex(index),
			    annotation = record.getValue("annotation"),
			    labels = this.get("uiLabels"),
			    tag = record.getValue("label"),
			    ov = this.deleteOverlay,
			    n = ov.get('srcNode');
			ov.set("headerContent", "<h3 class='delete_dialog'>"+ labels.deleteLabel + " " + tag +"</h3>");
			n.one('.delete-comment-input').detach();
			n.one('#confirm-delete').detach();
			n.one('#cancel-delete').detach();

			n.one('.delete-comment-input').on("key", this.onDelete, "enter", this, annotation, index);
			n.one('#confirm-delete').on("click", this.onDelete, this, annotation, index);
			n.one('#cancel-delete').on("click", this.onCancelDelete, this);

			ov.show();
			n.one('.delete-comment-input').focus();
		},

		onCommentAnnotation : function(e, tag) {
			var tags = this.tags;
			var record = tag;
			var index = this.tags.indexOf(record);
			var annotation = record.getValue("annotation");
			var labels = this.get("uiLabels");
			var title = record.getValue("title");
			var ov = this.commentOverlay;
			var n = ov.get('srcNode');

			ov.set("headerContent", "<h3 class='add_dialog'>"+ labels.commentLabel + " " + title +"</h3>");
			n.one('.tag-comment-input').detach();
			n.one('#confirm-tag-comment').detach();
			n.one('#cancel-tag-comment').detach();

			n.one('.tag-comment-input').on("key", this.onSubmitComment, "enter", this, annotation, index);
			n.one('#confirm-tag-comment').on("click", this.onSubmitComment, this, annotation, index);
			n.one('#cancel-tag-comment').on("click", this.onCancelComment, this);

			ov.show();
			n.one('.tag-comment-input').focus();
		},

		onCancelDelete : function() {
				Y.log('onCancelDelete');
				Y.one('.delete-comment-input').detach("key", this.onDelete, "enter");
				Y.one('.delete-comment-input').set("value", "");
				Y.one('#confirm-delete').detach("click", this.onDelete);
				Y.one('#cancel-delete').detach("click", this.onCancelDelete);
				this.deleteOverlay.hide();
			     },

		onCancelComment : function() {
				Y.log('onCancelcomment');
				Y.one('.tag-comment-input').detach("key", this.onSubmitComment, "enter");
				Y.one('.tag-comment-input').set("value", "");
				Y.one('#confirm-tag-comment').detach("click", this.onSubmitComment);
				Y.one('#cancel-tag-comment').detach("click", this.onCancelComment);
				this.commentOverlay.hide();
			     },

		onDelete : function (e, annotation, index) {
			var ov = this.deleteOverlay;
			var type = "tag";
			if (ov && index >= 0) {
			    var n = ov.get('srcNode');
		            var commentNode =  n.one('.delete-comment-input');
			    var comment = commentNode.get("value");
			    commentNode.set("value", "");
			    n.one('.delete-comment-input').detach("key", this.onDelete, "enter");
			    n.one('#confirm-delete').detach("click", this.onDelete);
			    n.one('#cancel-delete').detach("click", this.onCancelDelete);
			    ov.hide();
			} else {
			    var tags = this.tags;
		            var comment = '';
			    var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode"));
			    var record = tags.getRecordByIndex(index);
			    var annotation = record.getValue("annotation");
			}
			this.deleteAnnotation(annotation, index, comment);
		},

		deleteAnnotation : function(annotation, index, comment) {
			var oSelf = this;
			Y.log('remove annotation '+annotation+' with comment: '+comment);
			Y.io(this.get("store.remove"), {
				data:{ annotation:annotation, comment:comment },
				on:{success: function(e) {
					delete oSelf.get("myMetaTags")[annotation];
					oSelf.tags.remove(index);
				   }
				}
			});
		},
		deleteMetaAnnotation : function(tagindex, metaindex, annotation, target, comment) {
			var oSelf = this;
			Y.log('remove meta annotation '+annotation+ ' on target ' + target +' with comment: '+comment);
			Y.io(this.get("store.remove"), {
				data:{ annotation:annotation, comment:comment },
				on:{success: function(e) {
					delete oSelf.get("myMetaTags")[target][metaindex];
					var record = oSelf.tags.getRecordByIndex(tagindex);
					oSelf.renderTags([record], tagindex);
					Y.log("deleted " + metaindex + " for " + target);
				   }
				}
			});
		},

		onCommentRemoveClick : function(e) {
		     var annotation = e.currentTarget.getAttribute('alt');
		     var tag = this.findMetaTagByAnnotation(annotation);
		     var target = tag.hasTarget;
		     var record = this.findRecordByAnnotation(target);
		     var tagindex = this.tags.indexOf(record);
		     var metaindex = tag.type=='comment'?tag.type:tag.hasBody.value;
		     this.deleteMetaAnnotation(tagindex, metaindex, annotation, target, "deleted by user");
		},

		findMetaTagByAnnotation : function(an) {
		     var mymeta = this.get('myMetaTags');
		     for (target in mymeta) {
		       tlist = mymeta[target];
		       for (type in tlist) {
			 a = tlist[type];
			 if (a.annotation == an) return a;
		       }
		     }
		     return null; // not found
		},

		findRecordByAnnotation : function(an) {
		     Y.log('findTagByTarget: ' + an);
		     for (var i=0; i< this.tags.size(); i++) {
		       var record = this.tags.getRecordByIndex(i);
		       if (record.getValue('annotation') == an) return record;
		     }
		     return null;
		},

		getTags : function() {
			    var target = this.get('target');
			    var field = this.get('field');
			    var oSelf = this;
			    Y.io(this.get("store.get"),
				 { data: {
					 hasTarget: target,
					 field:  field
					 },
				   on: {
				       success: function(e,o) {
						  var r = Y.JSON.parse(o.responseText);
						  if (r && r[field] && r[field].annotations) {
							var ans = r[field].annotations;
							var len = ans.length;
							var user = oSelf.get('user');
							var myMetaTags = oSelf.get('myMetaTags');
							for (var i=0; i<len; i++) {
								var annotation_target = ans[i].hasTarget;
								var annotation_value = ans[i].hasBody.value;
								var annotation_type = ans[i].type;
								var annotation_user = ans[i].annotator;
								if (target != annotation_target	&& user == annotation_user) {
									if (!myMetaTags[annotation_target])
									  myMetaTags[annotation_target] = {};
									if (annotation_type == "comment")
									  myMetaTags[annotation_target][annotation_type] = ans[i];
									else
									  myMetaTags[annotation_target][annotation_value] = ans[i];
								}
							}
							oSelf.set('myMetaTags', myMetaTags);

							for (var i=0; i<len; i++) {
								annotation_target = ans[i].hasTarget;
								if (target == annotation_target) {
								        var tag = ans[i];
									var ovBody = oSelf.formatTagOverlay(tag);
									tag['overlay'] = new Y.Overlay({bodyContent: ovBody});
									oSelf.tags.add(ans[i]); // normal tag
								}
							}
						  }
						}
				       }
				 }
				);
			  },
		onSuggestionHover : function(e) {
			var infoNode = this.infoNode,
				active = e.newVal,
				body = '';
			if(active && active.getData().result.raw.info) {
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
				                      points:[Y.WidgetPositionAlign.TR, Y.WidgetPositionAlign.TL]});
				infoNode.show();
			} else {
			       infoNode.hide();
			}
		},
		onItemSelect : function(e) {
			Y.log('onItemSelect');
			var type = 'tag';
			if (e.preventDefault) e.preventDefault();
			var item = e.details[0].result.raw;
			this.setKeyInputHandler(true);
			var now = new Date();
			var delta = now - this.get("startTyping");
			var target = this.get("target");
			if (item.uri && item.label) {
			  this.submitAnnotation(type, target, {type:"uri", value:item.uri }, item.label, delta, null);
			} else {
			  this.submitAnnotation(type, target, {type:"literal", value: item}, item,       delta, null);
			};
			this.get("inputNode").set("value", "");
		},

		onTextSubmit : function(e, next) {
			Y.log('onTextSubmit');
			if (e.preventDefault) e.preventDefault();
			this.setKeyInputHandler(true);
			if(!this.get("activeItem")) {
			        var type = 'tag';
				var now = new Date();
				var delta = now - this.get("startTyping");
				var value = this.getTag();
				var target = this.get("target");
				this.submitAnnotation(type, target, {type:"literal", value:value}, value, delta, next);
			}
		},

		getTag: function() {
			      var value = this.get("inputNode").get("value");
			      this.get("inputNode").set("value", "");
			      return value?value:'';
		},

	        onJudgeAnnotation : function (ev, action, value, record) {
			var index = this.tags.indexOf(record);
			var target = record.getValue("annotation");
			var type = "judgement";
			var meta = this.get('myMetaTags')[target];
			for (var prop in meta) {
				if (meta[prop].type == "judgement") {
				  var old_annotation = meta[prop].annotation;
				  this.deleteMetaAnnotation(index, prop, old_annotation, target, "overruled by new "+value+" judgement");
				}
			}
			if (action == 'add') this.submitAnnotation(type,
								   target,
								   {type:"literal", value: value},
								  value,
								  -1,
								  null);
		},

		onSubmitComment : function(ev, ann, index) {
			var ov = this.commentOverlay;
			if (!ov) return;
			ov.hide();
			var type = "comment";
			var n = ov.get('srcNode');
			var commentNode =  n.one('.tag-comment-input');
			var comment = commentNode.get("value");
			commentNode.set("value", "");
			n.one('.tag-comment-input').detach("key", this.onCommentAnnotation, "enter");
			n.one('#confirm-tag-comment').detach("click", this.onCommentAnnotation);
			n.one('#cancel-tag-comment').detach("click", this.onCancelComment);
			this.submitAnnotation(type, ann, {type:"literal", value:comment}, comment, -1, null);
		},

		submitAnnotation : function(type, target, body, label, timing, next) {
		        if (!target) return;
		        if (!body.value) return;
			if (!label) label = body.value;
			if (!timing) timing = -1;
			if (!type) type = 'default';
			Y.log('add tag: '+ body.value +' with label: '+label+ ', time: ' + timing);

			var inputNode = this.get("inputNode");
			var tags = this.tags;
			var myMetaTags = this.get("myMetaTags");
			var oSelf = this;

			Y.io(this.get("store.add"), {
				data:{
					field:this.get("field"),
					hasTarget:target,
					hasBody:Y.JSON.stringify(body),
					label:label,
					typing_time: timing,
					type: type
				},
				on:{success: function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					if (type == "tag") {
					  var ovBody = oSelf.formatTagOverlay(r);
					  r['overlay'] = new Y.Overlay({bodyContent: ovBody});
					  tags.add(r);
					} else {
						var values = tags.getValuesByKey('annotation');
						var index = values.indexOf(target);
						if (!myMetaTags[target]) myMetaTags[target] = {}
						if (type == "judgement") {
						  myMetaTags[target][body.value] = r;
						} else if (type == "comment") {
						  myMetaTags[target][type] = r;
						}
						oSelf.set('myMetaTags', myMetaTags);
						var record = tags.getRecordByIndex(index);
						var tag = record.getValue();
						var ovBody = oSelf.formatTagOverlay(tag);
						delete tag.overlay;
						tag['overlay'] = new Y.Overlay({bodyContent: ovBody});
						tags.update(record, index);
					}
					if (next) {
					  Y.one('#' + next).focus();
					} else {
					  inputNode.focus();
					}
				    }
				}
			});
		},

		createDeleteNode : function(parentNode) {
			var Node = new Y.Overlay({}).render(parentNode);
			Node.hide();
			var labels = this.get('uiLabels');
			var head = "";
			var body = "<div class='annotate-comment delete-comment'>";
			body += "<h3>" + labels.commentLabel + "</h3>";
			body += "<textarea class='annotate-comment-input delete-comment-input' />";
			var foot  = "<button id='cancel-delete'>" + labels.cancelDeleteLabel + "</button>";
			foot += "<button id='confirm-delete'>" +labels.confirmDeleteLabel+ "</button>";
			Node.set("headerContent", head);
			Node.set("bodyContent",   body);
			Node.set("footerContent", foot);
			Node.set("centered", true);
			Node.set("width", "30em");
			this.deleteOverlay = Node;
		},

		createCommentNode : function(parentNode) {
			if (! this.get('commentEnabled')) return;
			var Node = new Y.Overlay({}).render(parentNode);
			Node.hide();
			var labels = this.get('uiLabels');
			var head = "";
			var body = "<div class='annotate-comment tag-comment'>";
			body += "<h3>" + labels.commentLabel + "</h3>";
			body += "<textarea class='annotate-comment-input tag-comment-input' />";
			var foot  = "<button id='cancel-tag-comment'>" + labels.cancelCommentLabel + "</button>";
			foot += "<button id='confirm-tag-comment'>" +labels.confirmCommentLabel+ "</button>";
			Node.set("headerContent", head);
			Node.set("bodyContent",   body);
			Node.set("footerContent", foot);
			Node.set("centered", true);
			Node.set("width", "30em");
			this.commentOverlay = Node;
		},

		// handlers to record typing time if relevant:
		setKeyInputHandler : function(first) {
			if (first) {
				this.get("inputNode").on(    "key", this.onFirstKey, 'press:', this);
			} else {
				this.get("inputNode").detach("key", this.onFirstKey, 'press:');
			}

		},
		onFirstKey : function(e) {
			if (e.button == 13) return; // ? not sure why I get the submit return here ...
			this.set("startTyping", new Date());
			var firstkey = false;
			this.setKeyInputHandler(firstkey);

		}

	});

	Y.Plugin.Annotation = Annotation;

}, '0.0.1', { requires: [
	'node','event','event-custom', 'autocomplete','overlay','recordset','io-base','json','querystring-stringify-simple'
	]
});
