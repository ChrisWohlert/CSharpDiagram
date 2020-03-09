$(document).ready(function(){
    $("#svg").find("svg").attr("preserveAspectRatio", "xMidYMid");

    $("#svg").click(function(){
    });

    $(".namespace").not(".class").click(function(){
        console.log($(this).find("path").attr("d"));
        [h, _, w, _] = $(this).find("path").attr("d").split(',')[2].split(' ');
        console.log(h, Math.abs(w));
        zoomToOffset($(this).offset(), Math.abs(w), Math.abs(h));
    });

    function zoomTo(x, y, w, h){
        const svg = $("#svg").find("svg");
        [x, y, w, h] = svg.attr("viewBox").split(' ').map(Number);
        let newViewBox = `${x} ${y} ${w * 0.9} ${h * 0.9}`
        svg.attr("viewBox", newViewBox);
    }

    function zoomToOffset(offset, w, h){
        console.log(offset, w);
        const svg = $("#svg").find("svg");
        svg.attr("viewBox", `${offset.left - 20} ${offset.top - 100} ${w} ${h + 100}`);
    }
});