// Generated by LispyScript v0.2.9
var floodfill = function(map,x,y) {
    return floodfill_dash(map,x,y,map_ref(map,x,y));
};
var map_ref = function(map,x,y) {
    return map[[x,",",y].join('')];
};
var floodfill_dash = function(map,_x,_y,val) {
    var acc = [];
    var rec = function(x,y) {
        return (((map_ref(map,x,y) !== val) || ((acc).indexOf([x,",",y].join('')) > -1)) ?
            acc :
            (function() {
                acc.push([x,",",y].join(''));
                rec((x - 1),y);
                rec((x + 1),y);
                rec(x,(y - 1));
                rec(x,(y + 1));
                return acc;
            })());
    };
    return rec(_x,_y);
};
