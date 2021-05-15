# How are color-concept associations formed? 

_Melissa A. Schoenlein & Karen B. Schloss_

Overview of paper





### Data files

CATEGORY LEARNING: 

`Exp1-catLearning` & `Exp2-catLearning`: store the alien category learning data for each experiment. Columns for each include: 
- `Subj`: participant number 
- `Ratings`: Association ratings [range: 0-1] 


ASSOCIATION RATINGS: 

`Exp1-associations`: store the association rating data for Experiment 1. Columns include: 
- `Subj`: participant number 
- `Ratings`: Association rating averaged over two presentations of a given color [range: 0-1] 
- `ColorID`: Hue of color 
- `ColorSetStr`: BCP classifications of colors as "saturated", "light", "muted", or "dark"
- `NameC`: Filk (0.5) or Slub (-0.5) species
- `WarmCoolSet`: Warm-biased species (1) or cool-biased species (-1)
- `Freq`: Co-occurrence frequency [range: 1-5]
- `FreqC`: Co-occurrence frequency mean centered [range: -2 to 2]
- `ShapeNameC`: species name & body shape [good fit: pointy Filks/curvy Slubs (0.5) vs. bad fit: curvy Filks/pointy Slubs (-0.5)]
- `ShapeColorC`: shape & color distribution [good fit: warm-biased pointy/cool-biased curvy (0.5) vs. bad fit: cool-biased pointy/warm-biased curvy(-0.5)]
- `Condition`: goodness-of-fit conditions: 
       (1) good shape-name fit & good shape-color fit;
       (2) good shape-name fit & bad shape-color fit;
       (3) bad shape-name fit & good shape-color fit;
       (4) bad shape-name fit & bad shape-color fit
- `UsedColorC`: responses to strategy questionnaire - participants who reported using color (0.5) vs. those who did not report using colors (-0.5) during the alien category learning task
- `NoticedColorStr`: responses to color patterns questionnaire - "noticers" vs. "non-noticers"
- `NoticedColorC`: responses to color patterns questionnaire - noticers (0.5) vs. non-noticers (-0.5)
- `ExposureStr`: whether the given color was "seen" vs. "unseen" during alien category learning
- `ExposureC`: whether the given color was seen (0.5) vs. unseen (-0.5) during alien category learning
- `deltaE`: Euclidean distance from the seen color for a given color category [range: 0 - 63]
- `deltaEC`: Euclidean distance from the seen color for a given color category, mean centered [range: -28 to 35]



 `Exp2-associations`:  store the association rating data for Experiment 2. Columns include: 
- `Subj`: participant number 
- `Ratings`: Association rating averaged over two presentations of a given color [range: 0-1] 
- `ColorID`: Hue of color 
- `ColorProtoF`: Hue of color and whether it was a prototype (P) or non-prototype (NP)
- `ProtoStr`: whether a color was a prototype (proto), non-prototype (non-proto), or achromatic (achr)
- `ProtoC`: whether a color was a prototype (0.5), non-prototype (-0.5), or achromatic (0)
- `Name`: species name, "Filk" or "Slub"
- `NameC`: Filk (0.5) or Slub (-0.5) species
- `ColorWarmnessC`: Warm colors [red,orange,yellow (.5)], cool colors [green, blue, purple (-0.5)], or achromatic colors [black, gray, white (0)]
- `FreqInFreqStr`: Whether color was seen frequently (Freq), infrequently (Infreq), or never (Achr) during alien category learning
- `FreqInFreqC`: Whether color was seen frequently (.5), infrequently (-.5), or never (0) during alien category learning
- `Freq`: Co-occurrence frequency [range: 1-5]
- `TypicalityStr`: saw prototypes (Saw P) vs. saw non-prototypes (Saw NP) during alien category learning: 
- `TypicalityC`: saw prototypes (.5) vs. saw non-prototypes during alien category learning: 
- `NoticedColorStr`: responses to color patterns questionnaire - "noticers" vs. "non-noticers"
- `NoticedColorC`: responses to color patterns questionnaire - noticers (0.5) vs. non-noticers (-0.5)
- `ExposureStr`: whether the given color was seen during alien category learning
- `ExposureC`: whether the given color was seen (0.5) vs. unseen (-0.5) during alien category learning
- `deltaE`: Euclidean distance from the seen color for a given color category [range: 0 - 60]





SUPPLEMENTAL DATA: 

`GoodFit-ColorName`: stores the data for the Goodness-of-fit color-name association task. Columns include: 

`GoodFit-ShapeColor`: stores the data for the Goodness-of-fit shape-color association task. Columns include: 

`GoodFit-ShapeName`: stores the data for the Goodness-of-fit shape-name color association task. Columns include: 



### R scripts

`CCAssociations`: Code for LMER analyses and plotting for all experiments (Exp. 1 & 2, Pilot, and Goodness-of-fit)





