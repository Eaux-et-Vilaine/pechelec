SELECT st_x(the_geom), * FROM vilaine.pechelec p 


Update  vilaine.pechelec SET (x,y) = (st_x(the_geom),st_y(the_geom))  WHERE id>=48;

