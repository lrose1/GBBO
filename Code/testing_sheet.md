Primacy vs. Recency in the Great British Bake Off
================

<!-- # ```{r setup, echo=FALSE} -->

<!-- #  -->

<!-- # knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file()) -->

<!-- # knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo=F) -->

<!-- #  -->

<!-- # ``` -->

## Question

Is it better to be first or last in the technical challenge of the Great
British Bake Off?

## Setup

The BBC television the *Great British Bake Off* (GBBO) is an amateur
baking competition that features a week-by-week elimination based on the
decisions of celebrity baker judges. Each week, there are three
challenges: a signature bake, a show-stopper, and a techincal challenge.

During the technical challenge, the bakers are given identical
ingredients and imprecise directions. Upon complettion, the bakers put
their creations behind their photos. The judges are then brought out to
judge the creations one by one, working from their right to
left:

![](https://i.dailymail.co.uk/i/pix/2016/08/24/21/378DDB3600000578-3757148-image-a-38_1472071296566.jpg
"Logo Title Text 1")

The technical challenge is the only one of the three weekly events that
features blind judging. Judges test the appearance, texture, and of
course, taste of each baker’s creation. We wondered if it was better to
be judged first (primacy effect), last (recency effect), or somewhere in
the middle (serial-position effect).

We hypothesized that being judged earlier would be better. Previous
tests of these effects focus on memory. But with GBBO, another factor
comes into play: hunger. Judges must eat a lot of baked goods,
especially in early weeks with more contestants. Perhaps the 12th bite
of something would not as satisfying as the first, disadvantaging bakers
judged earlier. Then again, maybe they have forgotten how good that
first bite was once they have had 11 more tastes.

## Results

#### Being judged first is good\!

Bakers judged first do about *10 percent* better in placements. This is
more pronounced in the first 4 weeks of each season, when being bakers
judged first do *17 percent* better.

Being judged in the first quarter of all bakers that week results in
placements that are *7 percent* better. They are also *8 percent* less
likely to fall into the bottom third of bakers in the technical
challenge that week.

#### Being judged in the middle not matter

Bakers judged in the middle do no worse or better in placements.

#### Being judged last might be good in early weeks

When there are lots of bakers still in contention, bakers judged last
might be a little less likely to go out that
week.

<!--html_preserve-->

<div class="muted well" style="width: 100% ; height: 500px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;">

Shiny applications not supported in static R Markdown documents

</div>

<!--/html_preserve-->

## Details

Most tests of serial-position effects can simply compare the order
judged to the placement. With GBBO, there is a different number of
