var WALL = 1,
    FLOOR = 0,
    DOOR = 2,
    EMPTY = 9;

Mapper = $.klazz({
  coded_coords: null,
  room_templates: null,
  xsize: null,
  ysize: null,

  initialize: function(xsize, ysize, room_templates) {
    this.xsize = parseInt(xsize, 10);
    this.ysize = parseInt(ysize, 10);
    this.room_templates = room_templates;
  },

  generate_coords: function(digCallback) {
    var tallest_height, widest_width, y, room_exits;

    this.digCallback = digCallback;
    this.coded_coords = [];

    // tallest room's height and widest rooms width?
    tallest_height = $.map(this.room_templates, function(rt) { return rt.height; } ).max();
    widest_width = $.map(this.room_templates, function(rt) { return rt.width; } ).max();

    y = 0;
    room_exits = [];

    // loop until our rows run out
    while( y < this.ysize ) {
      var rooms, room_for_x, a_room, row_tallest_height, gaps, num_unused_spaces, x;

      // pick some rooms
      rooms = [];
      room_for_x = this.xsize;

      while( room_for_x > widest_width ) {
        a_room = this.room_templates[Math.floor(Math.random() * this.room_templates.length)];
        rooms.push(a_room);
        room_for_x -= (a_room.width + 1);
      }

      // tallest height just for this row
      row_tallest_height = $.map(rooms, function(r) { return r.height; } ).max();

      // 0 spaces on each end, at least 3 spaces in between each room
      arr = new Array(rooms.length - 1);
      $.each(arr, function(i, v) { arr[i] = 3; });
      gaps = [0].concat(arr).concat([0]);

      num_unused_spaces = this.xsize - (3 * rooms.length + $.map(rooms, function(r) { return r.width; }).sum());

      // randomly distribute extra spaces into gaps
      for(i=0; i < num_unused_spaces; i++) {
        gaps[Math.floor(Math.random() * gaps.length)] += 1;
      }

      // prepopulate this room-row with blanks
      for(n=y; n < (y + tallest_height); n++) {
        arr = new Array(this.xsize);
        $.each(arr, function(i, v) { arr[i] = EMPTY; });
        this.coded_coords[n] = arr;
      }

      // shift ahead past first gap
      x = gaps[0];
      me = this;
      // cycle through rooms and populate coded_coords
      $.each(rooms, function(index, room) {
        var extra_y_offset = Math.floor(Math.random() * (row_tallest_height - room.height));

        $.each(room.coded_coords, function(y_in_room, room_y_row) {
          $.each(room_y_row, function(x_in_room, room_value) {
            var room_int = parseInt(room_value);
            if (isNaN(room_int)) room_int = EMPTY;
            digCallback(x + x_in_room, y + y_in_room + extra_y_offset, room_int); /////////////
            me.coded_coords[y + y_in_room + extra_y_offset][x + x_in_room] = room_int;
          });
        });


        // collect the exit points, offset for x, y
        $.each(room.exits(), function(i, e) {
          room_exits = room_exits.concat([[e[0]+x, e[1]+y+extra_y_offset]]);
        });

        // shift past this room and then next gap
        x += room.width + gaps[index+1];
      });

      y += tallest_height;
      if( y < this.ysize ) {
        for(n=y; n <= (y + 3); n++) {
          arr = new Array(this.xsize);
          $.each(arr, function(i, v) { arr[i] = EMPTY; });
          this.coded_coords[n] = arr;
        }
        y += 3;
      }

      // shift y up to one spot past what we have now, for the while loop
      y = this.coded_coords.length;

    } // end while loop through room rows

    // collect exit pairs - get the actual stub that sticks out from an exit if one exists
    var usable_room_exit_pairs = [];
    var the_mapper = this;

    $.each(room_exits, function(exit_index, exit) {
      var _wef = the_mapper.wheres_empty_from(exit[0],exit[1]);
      if(_wef) usable_room_exit_pairs.push([exit, _wef]);
    });

    // randomly sort the exit pairs
    usable_room_exit_pairs.sort( function() { return 0.5 - Math.random(); } );

    var used_exits = [];

    // now draw corridors by looping through all possible links between exits
    $.each(usable_room_exit_pairs, function(exit_index, exit_pair) {
      var other_exit_pairs = usable_room_exit_pairs.slice();
      other_exit_pairs.splice(exit_index, 1);
      $.each(other_exit_pairs, function(other_exit_index, other_pair) {
        var other_orig = other_pair[0];
        var other_outer_exit = other_pair[1];
        var this_orig = exit_pair[0];
        var outer_exit = exit_pair[1];
        if( the_mapper.is_clear_from_to(outer_exit[0], outer_exit[1], other_outer_exit[0], other_outer_exit[1]) ) {
          the_mapper.draw_corridor_from_to(outer_exit[0], outer_exit[1], other_outer_exit[0], other_outer_exit[1], digCallback);

         // digCallback(this_orig[0], this_orig[1], DOOR);   //mod - original one doesn't have visual door
         // digCallback(other_orig[0], other_orig[1], DOOR);
          the_mapper.coded_coords[this_orig[1]][this_orig[0]] = FLOOR;
          the_mapper.coded_coords[other_orig[1]][other_orig[0]] = FLOOR;

          used_exits.push(this_orig);
          used_exits.push(other_orig);
        }
      });
    });

    this.surround_every_floor_with_wall();

  },

  toText: function() {
    return $.map(this.coded_coords, function(cc) { return cc.join(''); }).join('\n');
  },

  toHTML: function() {
    return $.map(this.coded_coords,
      function(cc) { return $.map(cc, function(c) {
        return '<div class="t ' + (c === WALL || c === FLOOR ? c : '') + '">' +
          (c === EMPTY ? '&nbsp;' : c) + '</div>'; }).join('');
      }).join('<br/>');
  },


  // actually picks the random points for the corridor given the start and end and modifies the 2D array
  draw_corridor_from_to: function(x1, y1, x2, y2, digCallback) {
    var h_mod, v_mod, x, y;
    h_mod = x1 < x2 ? 1 : -1;
    v_mod = y1 < y2 ? 1 : -1;
    x = x1;
    y = y1;

    while( x !== x2 || y !== y2) {
      digCallback(x, y, FLOOR);
      this.coded_coords[y][x] = FLOOR;
      if(x != x2 && Math.random() > 0.5) {
        x += h_mod;
      } else if(y != y2) {
        y += v_mod;
      }
    }

    digCallback(x,y, FLOOR);
    this.coded_coords[y][x] = FLOOR;
  },

  /* utility functions to assist with mapping */

  wheres_empty_from: function(x,y) {
    if(this.north_from(x,y) === EMPTY) return [x,   y-1];
    if(this.south_from(x,y) === EMPTY) return [x,   y+1];
    if(this.west_from(x,y)  === EMPTY) return [x-1, y  ];
    if(this.east_from(x,y)  === EMPTY) return [x+1, y  ];
    return false;
  },

  north_from: function(x,y) { return (y > 0 ? this.coded_coords[y-1][x] : null); },

  south_from: function(x,y) { return (y < (this.coded_coords.length-1) ? this.coded_coords[y+1][x] : null); },

  west_from: function(x,y) { return (x > 0 ? this.coded_coords[y][x-1] : null); },

  east_from: function(x,y) { return (x < (this.coded_coords[0].length-1) ? this.coded_coords[y][x+1] : null); },

  northeast_from: function(x,y) { return ((y > 0 && x > 0) ? this.coded_coords[y-1][x+1] : null); },

  southeast_from: function(x,y) { return ((y > 0 && x > 0) ? this.coded_coords[y+1][x+1] : null); },

  northwest_from: function(x,y) { return ((y > 0 && x > 0) ? this.coded_coords[y-1][x-1] : null); },

  southwest_from: function(x,y) { return ((y > 0 && x > 0) ? this.coded_coords[y+1][x-1] : null); },

  // returns true only if you have a rectangle of empty spaces between the two points given
  is_clear_from_to: function(x1, y1, x2, y2) {
    start_x = [x1,x2].min();
    end_x = [x1,x2].max();
    start_y = [y1,y2].min();
    end_y = [y1,y2].max();
    for(y=start_y; y <= end_y; y++) {
      for(x=start_x; x <= end_x; x++) {
        if(this.coded_coords[y][x] !== EMPTY) return false;
      }
    }
    return true;
  },

  wall_around_corridor: function (x, y) {

      this.coded_coords[y][x] = WALL;
      this.digCallback(x,y,WALL);
  },

  // makes sure there are walls where appropriate - do after drawing corridors
  surround_every_floor_with_wall: function() {
    var the_mapper = this;

    $.each(this.coded_coords, function(y, y_row) {
      $.each(y_row, function(x, tile) {
        if(tile === FLOOR) {
          if(the_mapper.north_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x,y-1);
          if(the_mapper.south_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x,y+1);
          if(the_mapper.west_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x-1,y);
          if(the_mapper.east_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x+1,y);
          if(the_mapper.northeast_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x+1,y-1);
          if(the_mapper.southeast_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x+1,y+1);
          if(the_mapper.northwest_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x-1,y-1);
          if(the_mapper.southwest_from(x,y) === EMPTY)
              the_mapper.wall_around_corridor(x-1,y+1);
        } else if(tile === DOOR) {
          the_mapper.digCallback(x, y, WALL);
          the_mapper.coded_coords[y][x] = WALL;
        }
      });
    });
  }
});


ROT.Map.Rangersheck = function (width, height) {
   return new Mapper(width, height, [
        new RoomTemplate(  '1112111\n'
                         + '1000002\n'
                         + '1000001\n'
                         + '1000001\n'
                         + '1112111'),
        new RoomTemplate(  ' 1121   \n'
                         + ' 10001111 \n'
                         + '1000000011\n'
                         + '2000000001 \n'
                         + '1100000001 \n'
                         + '1100000002 \n'
                         + ' 100011111 \n'
                         + '  1111  '),
        new RoomTemplate( '111111111111121112111\n'
                        + '100000000000000000001\n'
                        + '100000000000000000001\n'
                        + '200000000000000000001\n'
                        + '100000000000000000001\n'
                        + '100000000000000000002\n'
                        + '111111111111111111111')
   ]);
};

Mapper.prototype.create = function (digCallback) {
    this.generate_coords(digCallback);
    //$(this.toHTML()).appendTo('#map_display');
};
