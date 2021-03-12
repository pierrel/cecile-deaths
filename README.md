In the Netflix show about the death at the Cecile hotel someone says something like "is there a room where someone didn't die?" This suggests (unreasonably) that a person died in all of the 700 rooms. How many people have to have died (assuming equal probably of death in each room) so that each room had a death?

To run:
clj -m cecile-deaths.core 10000 700 > data.in

Then in gnuplot: plot 'data.in'
