$(document).ready(function(){
    const svg = $("#svg").find("svg");
    svg.attr("preserveAspectRatio", "xMidYMid");

    const zoomables = [];

    $(".namespace").not(".class").each(function(i, element){
        const pos = getPositionForZoomable(element);
        if(pos !== undefined)
            zoomables.push(pos);
    });

    console.log(zoomables);

    $("#svg").click(function(){
    });

    $(".namespace").not(".class").click(function(){
        const d = $(this).find("path").attr("d");
        let pos = zoomables.filter(z => z.d == d)[0];
        console.log(pos);
        zoomTo(pos);
    });

    function zoomTo(pos){
        svg.attr("viewBox", `${pos.left} ${pos.top - 100} ${pos.width} ${pos.height + 100}`);
        $(this).scrollTop(0);
    }

    function getPositionForZoomable(element) {
        const d = $(element).find("path").attr("d");
        if(d !== undefined){
            [h, _, w, _] = d.split(',')[2].split(' ');
            return { left: $(element).offset().left, top: $(element).offset().top, width: Math.abs(w), height: Math.abs(h), d: d };
        }else {
            return undefined;
        }
    }
});