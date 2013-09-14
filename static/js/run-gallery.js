$(document).ready(function() {
	$(".fancybox-thumb").fancybox({
		helpers	: {
			title	: {
				type: 'outside'
			},
			thumbs	: {
				width	: 50,
				height	: 50
			}
		}
	});
});
window.onload = function() {
    $('#masContainer').masonry({
        columnWidth:110,
        itemSelector:'.img'
    });
};
