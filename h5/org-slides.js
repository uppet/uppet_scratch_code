
function asArticle(i) {
    var a = $('<article />').append($(this).contents());
	a.attr('class', $(this).attr('class'));
    $(this).replaceWith(a);
}

function wrapSectionMain(i) {
    var a = $('<section class="slides layout-regular template-default"/>').append($(this).contents());
	var b = $('<body>');
	b.append(a);
    $(this).replaceWith(b);
}

function addTagClass(i) {
	$(this).parent().addClass($(this).text());
	$(this).detach();
}

function addBigLogo(i) {
	$(this).parent().addClass("biglogo");
}

var re_build = /:build:/;
function parseBuild(i) {
	var t =$(this).text();
	if (t.match(re_build)) {
		t = t.replace(re_build, '');
		$(this).text(t);
		$(this).parent().addClass("build");
	}
}



function addBuild(i) {
	$(this).addClass("build");
}


$(document).ready(function () {
    $("body").css("display:none");
	$("body").each(wrapSectionMain);
	$("div#preamble").detach();
	$("div#postamble").detach();
	$("div#content > h1.title").detach();
	
    $("div#content > div.outline-2").each(asArticle);
	$("section.slides").append($("div#content").contents());
	$("div#content").detach();

	$("span.tag").each(addTagClass);

	$(".biglogo").each(addBigLogo);

	$(".build").each(addBuild);
	$("li").each(parseBuild);
});
