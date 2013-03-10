RoomTemplate = $.klazz({
  coded_coords: null,
  height: null,
  width: null,
  initialize: function(text) {
    var me, rows, long_x;

    this.coded_coords = [];

    rows = text.replace(' ', '9').split('\n');
    me = this;
    $.each(rows, function(r_index, row) {
      me.coded_coords[r_index] = row;
    });

    long_x = $.map(me.coded_coords, function(r) { return r.length; } ).max();

    $.each(me.coded_coords, function(y, y_row) {
      while(me.coded_coords[y].length < long_x) {
        me.coded_coords[y] += '9';
      }
    });

    this.height = this.coded_coords.length;
    this.width = this.coded_coords[0].length;
  },

  exits: function() {
    var tiles = [];

    $.each(this.coded_coords, function(y, y_row) {
      $.each(y_row, function(x, val) {
        if(val==="2") tiles.push([x,y]);
      });
    });
    return tiles;
  }
});
