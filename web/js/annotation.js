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
	target:			{ value: null }, // URI of target to be annotated
	targetImage:		{ value: null }, // URI of target's image to be annotated
	field:			{ value: null }, // URI identifying annotation field
	store:			{ value: null }, // URIs of web services to CRUD http annotation api
	startTyping:		{ value: null }, // timestamp when users start typing
	myMetaTags:		{ value: {} },   // myMetaTags dictionary
	uiLabels:		{ value: [] },   // dictionary with ui labels in the prefered language of the user
	imageId:                { value: null }, // id of the corresponding img element
	next:			{ value: null }, // id of next element to tab to in the tabindex
	user:                   { value: "anonymous" },
	
	// configuration options:
	tagStyle:          	{ value: "overlay" },   // show tag details inline, on overlay, or simple (none)
	deleteEnabled:          { value: "mine" },   // when delete icon is shown each tag
	commentEnabled:		{ value: "always" }, // when comment icon is shown for each tag
	unsureEnabled:		{ value: "always" }, // when "I'm not sure" checkboxes will be shown for each tag
	agreeEnabled:		{ value: "yours" },  // when "I agree" checkboxes will be shown for each tag
	disagreeEnabled:	{ value: "yours" },  // when "I disagree" checkboxes will be shown for each tag
	deleteCommentEnabled:	{ value: "always" }, // when comment overlay is shown for deletions on this field
	tagFilter:		{ value: "always" },   // hack for roles exp: do not show tag with wrong user field 
    };

    Annotation.MOTIVATION = {
	tagging:    'http://www.w3.org/ns/oa#tagging',
	commenting: 'http://www.w3.org/ns/oa#commenting',
	moderating: 'http://www.w3.org/ns/oa#moderating',
    };

    Annotation.LIST_CLASS = 'taglist';
    Annotation.LIST_TEMPLATE = '<ul class="'+Annotation.LIST_CLASS+'"></ul>';
    
    Y.extend(Annotation, Y.Plugin.AutoComplete, {
	
		initializer: function(args) {
			var next = this.get("next");
			var parentNode = this.DEF_PARENT_NODE;
		
			if (typeof(anno) != "undefined")
				this._anno = anno; // hack
			else
				this._anno = null;

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

			// this.on('myMetaTagsChange', function(ev) {Y.log('myMetaTagsChange'); Y.log(ev);});

			// create tagList node (tag view in mvc)
			this.tagList = Y.Node.create(Annotation.LIST_TEMPLATE);
			this.tagList.addClass(this.get('tagStyle'));
			parentNode.append(this.tagList);


			// infoNode is the overlay with tooltips when hovering over suggested terms
			this.infoNode = new Y.Overlay({}).render(parentNode);
			this.on("activeItemChange",  this.onSuggestionHover, this);
			this.on("hoveredItemChange", this.onSuggestionHover, this);

			// create overlays for comments on delete and add actions:
			this.createCommentNode(parentNode);
			if (this.enabled('deleteCommentEnabled', null)) {
				Y.delegate("click", this.onTagRemoveClick, this.tagList, 'li .tagremove.enabled', this);
				this.createDeleteNode(parentNode);
			} else {
				Y.delegate("click", this.onDelete, this.tagList, 'li .tagremove.enabled', this);
			}
			Y.delegate("click", this.onMetaRemoveClick, this.tagList, '.metaremove a', this);


			// setup handlers to record typing time:
			var firstkey = true;
			this.setKeyInputHandler(firstkey);

			var oSelf = this;
			Y.on("load", function(e) { oSelf.getTags(); });
		},

	    findTarget : function(tag, type) {
		if (type == 'specific')
		    return this.findSpecificTarget(tag)
		else
		    return this.findGenericTarget(tag)
	    },

	    findGenericTarget : function(tag) {
		var targets = tag.hasTarget;
		var target = undefined;
		if (!targets) 
		    return null;
		if (targets['@id'])  
		    return targets;
		for (var t in targets) {
		    target = targets[t];
		    if (target['@id']) 
			return target;
		}
		return null
	    },

	    findSpecificTarget : function(tag) {
		var targets = tag.hasTarget;
		var target = undefined;
		if (!targets) 
		    return null;
		if (targets.hasSelector) 
		    return targets;
		for (var t in targets) {
		    target = targets[t];
		    if (target.hasSelector)
			return target;
		}
		return null;
	    },

	    addTagFragment : function(tag, update) {
		var target = this.findTarget(tag, 'specific');
		if (! this._anno || !target) return;
	
		var label   = tag.title;
		var x =  target.hasSelector.x;
		var y =  target.hasSelector.y;
		var w =  target.hasSelector.w;
		var h =  target.hasSelector.h;
		var torious = { 
		    src: Y.one('img#'+this.get('imageId')).get('src'),
		    text: label,
		    targetId: target['@id'],
		    annotationId: tag.annotation,
		    shapes: [{
			type:'rect', 
			geometry: { x:x,y:y,width:w,height:h }
		    }]
		};
		this._anno._deniche.addAnnotation(torious, update);
	    },

	    removeTagFragment : function(tag) {
		var target = this.findTarget(tag, 'specific');
		if (!target) return;
		this._anno._deniche.removeAnnotation(tag.title, target['@id']);
	    },

	    // handlers for adding additions, updates or removals in the tag Recordset:
	    addTags : function(o) {
		this.renderTags(o.added, o.index);

	    },

		updateTags : function(o) {
			    this.renderTags(o.updated, o.index);
	        },

		removeTags : function(o) {
			var tagNodes = this.tagList.all("li");
			for (var i=o.index; i < o.index+o.range; i++) {
				var node = tagNodes.item(i);
				node.one('.taglabel').detach('hover');
				node.remove();
			}
		},

		renderTags : function(tags, index) {
			var tagList = this.tagList;
			var tagStyle = this.get('tagStyle');
			var tagNodes = this.tagList.all("li");
			if (index < tagNodes.size()) {
				this.removeTags({index:index, range:tags.length});
			}
			// format the tags
			for(var i=0; i < tags.length; i++) {
				var tag=tags[i].getValue();
				var node = Y.Node.create('<li class="tagitem">'+this.formatTag(tags[i], tagStyle)+'</li>');
				node.setAttribute('targetId', tags[i].getValue('targetId'));
				node.all('.judgeButton').addClass(tagStyle);
				tagList.insert(node, index+i);
				if (tagStyle == 'overlay')
					node.one('.taglabel').on('hover', this.onTagHover, this.onTagHover, this, tags[i]);
				else if (tagStyle == 'inline') {
					this.rebindButtons(node, tags[i]);
				}
			};
		},

		enabled : function(option, tag) {
			var when   = this.get(option);
			var user   = this.get("user");

			if (when == "always") return true;
			else if (when == "never") return false;

			var tag_author = tag.annotatedBy?tag.annotatedBy:"no_tag_author!";

			if (when == "mine")
			  return (user == tag_author);
			else if (when == "yours")
			  return (user != tag_author);
			else if (when == "user") { // hack for roles experiment:
			  var tag_user   = tag.user?tag.user:tag_author;
			  return (user == tag_user);
			}
			else {
				Y.log(option + ' not implemented in enabled()');
				return false;
			}

		},

		formatTag : function(tagrecord, tagStyle) {
			var html = "";
			var label  = tagrecord.getValue("title");

			if (this.enabled('deleteEnabled', tagrecord.getValue())) {
			  html += '<div class="tagremove enabled"><a href="javascript:{}">x</a></div>';
			} else {
			  html += '<div class="tagremove disabled"><a href="javascript:{}">y</a></div>';
			}
			if (tagStyle == "overlay")
				html += "<div class='overlay taglabel'>" + label + "</div>";
			else if (tagStyle == "simple")
				html += "<div class='simple taglabel'>" + label + "</div>";
			else if (tagStyle == "inline") {
				var judgement_buttons = this.formatJudgmentButtons(tagrecord.getValue());
				var buttons = '<div class="inline commentButtons">' + judgement_buttons + '</div>';
				html += buttons;
				html += "<span class='inline taglabel'>" + label + "</span>";
			}
			if (tagStyle != "overlay") {
				var user   = this.get("user");
				var annotatedBy = tagrecord.getValue("annotatedBy");
				if (user != annotatedBy) {
					var screenName = tagrecord.getValue("screenName");
					var credit = this.get('uiLabels').tagCreditLabel;
					html += "<span class='inline screenName'>" 
					if (credit)
						html += "<span class='inline tagCreditLine'>" + credit + "</span>";
					html +=	screenName + "</span>";
				}
			}
			return html;
		},

		formatTagOverlay : function(tag) {
			var label = tag.title;
			var link  = tag.display_link;
			var annot = tag.annotation;
			var mymeta     = this.get('myMetaTags')[annot];
			var screenName = tag.screenName;

			var judgement_buttons = this.formatJudgmentButtons(tag);
			var buttons = '<div class="commentButtons">' + judgement_buttons + '</div>';
			var html = '<div class="overlay tagCreation">';
			html +=	'<div class="overlay title taglabel">';
			if (link == '')
				html += label;
			else
				html += '<a href="'+link+'">'+label+'</a>';
			html += '</div>'; // end label div
			html += '<div class="overlay screenName">'+screenName+'</div>';
			html += '</div>'; // end tagCreation div


			if (mymeta && mymeta[Annotation.MOTIVATION.commenting]) {
			  var comment = mymeta[Annotation.MOTIVATION.commenting];
			  html += '<div class="overlay comment">';
			  // html += '<span class="screenName">' + comment.screenName + "</span>";
			  if (this.enabled('deleteEnabled', comment)) {
			    html += '<span class="metaremove"><a class="metaremove" alt="' + comment.annotation + '">x</a></span>';
			  }
			  html += '<span class="body">' + comment.title + "</span>";

			  html += '</div>';
			}

			html += buttons;

			return html;
		},

		formatJudgmentButtons : function(tag) {
			var my_rating_label = '';
			var my_rating = null;
			var annot = tag.annotation;
			var mymeta = this.get('myMetaTags')[annot];
			var judgement_buttons = '';
			if (this.enabled('commentEnabled', tag)) {
				var commentLabel = this.get('uiLabels').commentLabel;
				judgement_buttons += "<span title='" + commentLabel + "' ";
			        judgement_buttons += "class='judgeButton unchecked commentButton'>";
				judgement_buttons += "<img src='./icons/bubble.png' title='" + commentLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('unsureEnabled', tag)) {
				var unsureLabel = this.get('uiLabels').unsureLabel;
				var unsure_value = undefined;
				if (mymeta && mymeta.unsure) unsure_value = mymeta.unsure.hasBody['@value'];
				var checked = 'unchecked';
				if (unsure_value != undefined){
				  checked = 'checked'; my_rating_label = unsureLabel; my_rating = mymeta.unsure;
				}
				judgement_buttons += "<span title='" + unsureLabel + "' ";
				judgement_buttons += "class='judgeButton unsureButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/unsure.png' title='" + unsureLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('disagreeEnabled', tag)) {
				var disagreeLabel = this.get('uiLabels').disagreeLabel;
				var disagree_value = undefined;
				if (mymeta && mymeta.disagree) disagree_value = mymeta.disagree.hasBody['@value'];
				var checked = 'unchecked';
				if (disagree_value != undefined){
				  checked = 'checked'; my_rating_label = disagreeLabel; my_rating = mymeta.disagree;
				}
				judgement_buttons += "<span title='" + disagreeLabel + "' ";
				judgement_buttons += "class='judgeButton disagreeButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/thumbDown.png' title='" + disagreeLabel + "'/>";
				judgement_buttons += "</span>";
			}
			if (this.enabled('agreeEnabled', tag)) {
				var agreeLabel = this.get('uiLabels').agreeLabel;
				var agree_value = undefined;
				if (mymeta && mymeta.agree) agree_value = mymeta.agree.hasBody['@value'];
				var checked = 'unchecked';
				if (agree_value != undefined) {
				  checked = 'checked'; my_rating_label = agreeLabel; my_rating = mymeta.agree;
				}
				judgement_buttons += "<span title='" + agreeLabel + "' ";
				judgement_buttons += "class='judgeButton agreeButton " + checked + "'>";
				judgement_buttons += "<img src='./icons/thumbUp.png' title='" + agreeLabel + "'/>";
				judgement_buttons += "</span>";
			}

			if (my_rating) {
			  judgement_buttons += '<div class="overlay rating">';
			  if (this.enabled('deleteEnabled', my_rating)) {
			    judgement_buttons += '<span class="metaremove"><a class="metaremove" alt="' + my_rating.annotation + '">x</a></span>';
			  }
			  judgement_buttons += '<span class="overlay ratinglabel">' + my_rating_label + '</span>';
			  judgement_buttons += '</div>';
			}
			return judgement_buttons;
		},

		onTagHover: function(ev, tagrecord) {
			// Y.log('onTagHover');
			var overlay = tagrecord.overlay;
			if (overlay) { 
				overlay.destroy(); 
			}
			if (ev.phase == 'over') {
			  var ovBody = this.formatTagOverlay(tagrecord.getValue());
			  overlay = new Y.Overlay({bodyContent: ovBody});
			  overlay.set('active', false);
			  overlay.set('width','25em');
			  overlay.render(ev.target);
			  tagrecord.overlay = overlay;
			  var node = overlay.get('srcNode');
			  this.rebindButtons(node, tagrecord);
			  node.all('.judgeButton').addClass("overlay");
			  overlay.show();
			  if (ev.pageX/window.innerWidth < 0.4)
			  	overlay.set("align", {node:ev.target, points:[Y.WidgetPositionAlign.LC, Y.WidgetPositionAlign.TL]});
			  else
			  	overlay.set("align", {node:ev.target, points:[Y.WidgetPositionAlign.CC, Y.WidgetPositionAlign.TL]});
			} 
		},

		rebindButtons: function(node, tagrecord) {
			node.all('.judgeButton').detach('click');
			node.one('.commentButton').on(           'click', this.onCommentAnnotation, this, tagrecord);

			node.all('.unsureButton.unchecked').on(  'click', this.onJudgeAnnotation, this, 'add', 'unsure',   tagrecord);
			node.all('.agreeButton.unchecked').on(   'click', this.onJudgeAnnotation, this, 'add', 'agree',    tagrecord);
			node.all('.disagreeButton.unchecked').on('click', this.onJudgeAnnotation, this, 'add', 'disagree', tagrecord);

			node.all('.unsureButton.checked').on(    'click', this.onJudgeAnnotation, this, 'rm',  'unsure',   tagrecord);
			node.all('.agreeButton.checked').on(     'click', this.onJudgeAnnotation, this, 'rm',  'agree',    tagrecord);
			node.all('.disagreeButton.checked').on(  'click', this.onJudgeAnnotation, this, 'rm', ' disagree', tagrecord);
		},

		onTagRemoveClick : function(e) {
			var index = this.tagList.all("li").indexOf(e.currentTarget.get("parentNode")),
			    tags = this.tags,
			    record = tags.getRecordByIndex(index),
			    annotation = record.getValue("annotation"),
			    labels = this.get("uiLabels"),
			    tag = record.getValue("title"),
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

	    onCommentAnnotation : function(e, tagrecord) {
		if (tagrecord.overlay) tagrecord.overlay.set('active', true);
		var tags = this.tags;
		var labels = this.get("uiLabels");
		var annotation = tagrecord.getValue("annotation");
		var target = this.findGenericTarget(tagrecord.getValue())['@id'];
		var title = tagrecord.getValue("title");
		var ov = this.commentOverlay;
		var n = ov.get('srcNode');
		console.log(target);
		
		ov.set("headerContent", "<h3 class='add_dialog'>"+ labels.commentLabel + " " + title +"</h3>");
		n.one('.tag-comment-input').detach();
		n.one('#confirm-tag-comment').detach();
		n.one('#cancel-tag-comment').detach();
		
		n.one('.tag-comment-input').on("key", this.onSubmitComment, "enter", this, annotation, target);
		n.one('#confirm-tag-comment').on("click", this.onSubmitComment, this, annotation, target);
		n.one('#cancel-tag-comment').on("click", this.onCancelComment, this);
		
		ov.show();
		n.one('.tag-comment-input').focus();
	    },

		onCancelDelete : function(ev) {
				if (ev.preventDefault) ev.preventDefault();
				Y.one('.delete-comment-input').detach("key", this.onDelete, "enter");
				Y.one('.delete-comment-input').set("value", "");
				Y.one('#confirm-delete').detach("click", this.onDelete);
				Y.one('#cancel-delete').detach("click", this.onCancelDelete);
				this.deleteOverlay.hide();
			     },

		onCancelComment : function(ev) {
				if (ev.preventDefault) ev.preventDefault();
				Y.one('.tag-comment-input').detach("key", this.onSubmitComment, "enter");
				Y.one('.tag-comment-input').set("value", "");
				Y.one('#confirm-tag-comment').detach("click", this.onSubmitComment);
				Y.one('#cancel-tag-comment').detach("click", this.onCancelComment);
				this.commentOverlay.hide();
			     },

		onDelete : function (ev, annotation, index) {
			if (ev.preventDefault) ev.preventDefault();
			var ov = this.deleteOverlay;
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
				method: 'DELETE',
				data:{ annotation:annotation, comment:comment },
				on:{success: function(e) {
					delete oSelf.get("myMetaTags")[annotation];
					var tag = oSelf.tags.getRecordByIndex(index).getValue();
					oSelf.removeTagFragment(tag);
					oSelf.tags.remove(index);
				   }
				}
			});
		},
		deleteMetaAnnotation : function(tagindex, metaindex, annotation, target, comment) {
			var oSelf = this;
			Y.log('remove meta annotation '+annotation+ ' on target ' + target +' with comment: '+comment);
			Y.io(this.get("store.remove"), {
				method: 'DELETE',
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

		onMetaRemoveClick : function(ev) {
		     var annotation = ev.currentTarget.getAttribute('alt');
		     var tag = this.findMetaTagByAnnotation(annotation);
		     var target = tag.hasTarget['@id'];
		     var record = this.findRecordByAnnotation(target);
		     var tagindex = this.tags.indexOf(record);
		     var metaindex = tag.motivatedBy==Annotation.MOTIVATION.commenting?tag.motivatedBy:tag.hasBody['@value'];
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

		getTags : function(ev, oSelf) {
			    Y.detach("load");
			    var targetURI = this.get('target');
			    var field = this.get('field');
			    var oSelf = this;
			    Y.io(this.get("store.get"), { 
				   method: 'GET',
				   data: {
					 hasTarget: targetURI,
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
								// look for relevant meta annotations
				    				var annotation_target_uri = ans[i].hasTarget['@id'];
								if (!annotation_target_uri) continue; // probably a fragment annotation

								if (targetURI == annotation_target_uri) continue // normal annotation
								var annotation_user = ans[i].annotatedBy;

								if (user != annotation_user) continue // meta annotation from other user

								var annotation_body = ans[i].hasBody['@id']?ans[i].hasBody['@id']:ans[i].hasBody['@value'];
								var annotation_motiv = ans[i].motivatedBy;
								if (!myMetaTags[annotation_target_uri])
								  myMetaTags[annotation_target_uri] = {};
								if (annotation_motiv == Annotation.MOTIVATION.commenting)
								  myMetaTags[annotation_target_uri][annotation_motiv] = ans[i];
								else
								  myMetaTags[annotation_target_uri][annotation_body] = ans[i];
							}
							oSelf.set('myMetaTags', myMetaTags);

							for (var i=0; i<len; i++) {
								var tag = ans[i];
								if (!oSelf.enabled('tagFilter', tag)) continue; // hack for exp usage

							    var selector_target = oSelf.findTarget(tag, 'specific');
	        						if (selector_target) {
									ans[i].targetId = selector_target['@id'];
									oSelf.tags.add(ans[i]); // normal image fragment tag
									oSelf.addTagFragment(ans[i], true);
								} else if (tag.hasTarget['@id'] == targetURI) {
									ans[i].targetId = targetURI;
									oSelf.tags.add(ans[i]); // normal object tag
								}
							}
						  }
						}
				       }
				 }
				);
			  },
		onSuggestionHover : function(ev) {
			var infoNode = this.infoNode,
				active = ev.newVal,
				body = '';
			if(active && active.getData().result.raw.info) {
				var scope = active.getData().result.raw.info.scopeNotes[0];
				var defin = active.getData().result.raw.info.definitions[0];
				var alts =  active.getData().result.raw.info.altLabels;
				var img =  active.getData().result.raw.info.images[0];
				if (scope && scope.en) { body += "<div class='scope'>"+scope.en+"</div>"; }
				if (defin && defin.en) { body += "<div class='defin'>"+defin.en+"</div>"; }
				if (img) { body += "<img class='depiction' src='"+img+"'>"; }
				for (var i=0; i<alts.length; i++) {
					body += "<span class='altLabel'>" +alts[i] + "</span>" ;
				}
			}
			if(body) {
				var ratio = ev.newVal.getX()/window.innerWidth;
			        if (ratio < 0.4) { // make suggestions appear on the left or the right:
			  		infoNode.set("align", {node:active, points:[Y.WidgetPositionAlign.TL, Y.WidgetPositionAlign.TR]});
				} else {
			  		infoNode.set("align", {node:active, points:[Y.WidgetPositionAlign.TR, Y.WidgetPositionAlign.TL]});
				}
				infoNode.set("bodyContent", body);
				infoNode.show();
			} else {
			       infoNode.hide();
			}
		},
		onItemSelect : function(ev) {
			// Y.log('onItemSelect');
			if (ev.preventDefault) ev.preventDefault();
			var motiv = Annotation.MOTIVATION.tagging;
			var item = ev.details[0].result.raw;
			this.setKeyInputHandler(true);
			var now = new Date();
			var delta = now - this.get("startTyping");
			var target = this.get('target');
			if (item.uri && item.label) {
			    this.submitAnnotation(motiv, target, {'@id':item.uri }, item.label, delta, null, target);
			} else {
			    this.submitAnnotation(motiv, target, {'@value': item},  item,       delta, null, target);
			};
			this.get("inputNode").set("value", "");
		},

		onTextSubmit : function(ev, next) {
			// Y.log('onTextSubmit');
			if (ev.preventDefault) ev.preventDefault();
			this.setKeyInputHandler(true);
			if(!this.get("activeItem")) {
				var motiv = Annotation.MOTIVATION.tagging;
				var now = new Date();
				var delta = now - this.get("startTyping");
				var value = this.getTag();
				var target = this.get('target');
			    this.submitAnnotation(motiv, target, {'@value':value}, value, delta, next, target);
			}
		},

		getTag: function() {
			      var value = this.get("inputNode").get("value");
			      this.get("inputNode").set("value", "");
			      return value?value:'';
		},

	    onJudgeAnnotation : function (ev, action, value, record) {
		if (ev.preventDefault) ev.preventDefault();
		if (record.overlay) record.overlay.set('active', false);
		var index = this.tags.indexOf(record);
		var target = record.getValue("annotation");
		var motiv = Annotation.MOTIVATION.moderating;
		var meta = this.get('myMetaTags')[target];
		for (var prop in meta) {
		    if (meta[prop].motivatedBy == Annotation.MOTIVATION.moderating) {
			var old_annotation = meta[prop].annotation;
			this.deleteMetaAnnotation(index, prop, old_annotation, target, "overruled by new "+value+" judgement");
		    }
		}
		graph = this.findGenericTarget(record.getValue())['@id'];
		if (action == 'add') this.submitAnnotation(motiv,
							   target,
							   {'@value': value},
							   value,
							   -1,
							   null,
							   graph);
	    },

	    onSubmitComment : function(ev, ann, graph) {
		var ov = this.commentOverlay;
		if (!ov) return;
		if (ev.preventDefault) ev.preventDefault();
		ov.hide();
		var motiv = Annotation.MOTIVATION.commenting;
		var n = ov.get('srcNode');
		var commentNode =  n.one('.tag-comment-input');
		var comment = commentNode.get("value");
		commentNode.set("value", "");
		n.one('.tag-comment-input').detach("key", this.onCommentAnnotation, "enter");
		n.one('#confirm-tag-comment').detach("click", this.onCommentAnnotation);
		n.one('#cancel-tag-comment').detach("click", this.onCancelComment);
		this.submitAnnotation(motiv, ann, {'@value':comment}, comment, -1, null, graph);
	    },

	    submitAnnotation : function(motiv, target, body, label, timing, next, graph) {
		if (next) {
		    Y.one('#' + next).focus();
		} else {
		    this.get("inputNode").focus();
		}
		
		if (!target) return;
		if (!body) return;
		if (!label && body['@value']) label = body['@value'];
		if (!timing) timing = -1;
		if (!motiv) motiv = motiv = Annotation.MOTIVATION.tagging;
		if (!graph) graph = target;

		var bodyString = Y.JSON.stringify(body);

		var targetObject = [{'@id':target}]
		if (this._anno && this._anno._deniche.currentShape) { 
		    var shape = this._anno._deniche.currentShape.geometry; 
		    var targetImage = this.get('targetImage');
		    if (targetImage && target != targetImage) {
			targetObject = [ { hasSource: targetImage, hasSelector: { value:shape}}, { '@id':target } ];
		    } else {
			targetObject = [ { hasSource: target, hasSelector: { value:shape}} ];
		    }
		}
		var targetString = Y.JSON.stringify(targetObject);
		
		var tags = this.tags;
		var myMetaTags = this.get("myMetaTags");
		var oSelf = this;
		
		Y.io(this.get("store.add"), {
		    method: "POST",
		    data:{
			field:this.get("field"),
			hasTarget:targetString,
			hasBody:bodyString,
			label:label,
			typing_time: timing,
			motivatedBy: motiv,
			graph: graph
		    },
		    on:{success: function(e,o) {
			var response = Y.JSON.parse(o.responseText);
			var r = response.annotation;
			if (motiv == Annotation.MOTIVATION.tagging) {
			    tags.add(r);
			    oSelf.addTagFragment(r, false); // add but do not update open editor
			} else {
			    var values = tags.getValuesByKey('annotation');
			    var index = values.indexOf(target);
			    if (!myMetaTags[target]) myMetaTags[target] = {}
			    if (motiv == Annotation.MOTIVATION.moderating) {
				myMetaTags[target][label] = r;
			    } else if (motiv == Annotation.MOTIVATION.commenting) {
				myMetaTags[target][motiv] = r;
			    }
			    oSelf.set('myMetaTags', myMetaTags);
			    var record = tags.getRecordByIndex(index);
			    tags.update(record, index);
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
			Node.set("width", "50em");
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
			Node.set("width", "50em");
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
