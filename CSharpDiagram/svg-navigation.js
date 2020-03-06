$(document).ready(function(){
    var svg = document.querySelector("#svg");
    svg.addEventListener('click', zoom, false);
    function zoom(){
        [x, y, w, h] = svg.getAttribute("viewBox").split(' ').map(Number);
        let newViewBox = `${x} ${y} ${w * 0.9} ${h * 0.9}`
        svg.setAttribute("viewBox", newViewBox);
    }
});