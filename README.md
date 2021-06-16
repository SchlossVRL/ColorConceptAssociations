# How are color-concept associations formed? 

_Melissa A. Schoenlein & Karen B. Schloss_

Color-concept associations influence fundamental processes in cognition and perception including object recognition, color perception, and visual reasoning. To understand these effects, it is necessary to understand how color-concept associations are formed in the first place. It is assumed that color-concept associations are learned through experiences in the world, but questions remain concerning how association formation is influenced by properties of the input and cognitive factors during exposure to that input. We addressed these questions by first exposing participants to color-concept co-occurrences for novel concepts using a category learning task and then assessing color-concept associations. Critically, color was a noisy cue during category learning and shape was 100% diagnostic, so participants could have ignored color to complete the task. We found that participants learned systematic color-concept associations for “seen” colors during category learning and generalized to other “unseen” colors depending on color distance from the seen colors. However, the nature of those associations depended on whether participants noticed color-related patterns during category learning. We also found that formed associations depended on cognitive structure of color categories. Thus, people are not merely “statistics detectors”; multiple environmental and cognitive factors combine to influence color-concept associations. 



### Data files


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
- `TypicalityC`: saw prototypes (.5) vs. saw non-prototypes during alien category learning 
- `NoticedColorStr`: responses to color patterns questionnaire - "noticers" vs. "non-noticers"
- `NoticedColorC`: responses to color patterns questionnaire - noticers (0.5) vs. non-noticers (-0.5)
- `ExposureStr`: whether the given color was seen during alien category learning
- `ExposureC`: whether the given color was seen (0.5) vs. unseen (-0.5) during alien category learning
- `deltaE`: Euclidean distance from the seen color for a given color category [range: 0 - 60]



CATEGORY LEARNING: 

`Exp1-catLearning`: stores the alien category learning data for Experiment 1. Columns include: 
- `Subj`: participant number 
- `ShapeNameC`: species name & body shape [good fit: pointy Filks/curvy Slubs (0.5) vs. bad fit: curvy Filks/pointy Slubs (-0.5)]
- `ShapeColorC`: shape & color distribution [good fit: warm-biased pointy/cool-biased curvy (0.5) vs. bad fit: cool-biased pointy/warm-biased curvy(-0.5)]
- `Condition`: goodness-of-fit conditions: 
       (1) good shape-name fit & good shape-color fit;
       (2) good shape-name fit & bad shape-color fit;
       (3) bad shape-name fit & good shape-color fit;
       (4) bad shape-name fit & bad shape-color fit
- `Trial`: trial number in experiment [range: 1-240]
- `TrialBin`: trials binned by every 40 [range: 1-6]
- `ResponseTime`: response time in ms
- `bodyShape`: pointy shape (1) or curvy shaped (2) alien body
- `Response`: participants left (1) or right (2) button press to categorize the alien
- `Correct`: whether the response was correct (1) or incorrect (0) to classify the alien
- `totalCorrect`: total number of correct trials for a given participant [range: 0-240]
- `NoticedColorStr`: responses to color patterns questionnaire - "noticers" vs. "non-noticers"
- `NoticedColorC`: responses to color patterns questionnaire - noticers (0.5) vs. non-noticers (-0.5)


`Exp2-catLearning`: stores the alien category learning data for Experiment 1. Columns include: 
- `Subj`: participant number 
- `TypicalityStr`: saw prototypes (Saw P) vs. saw non-prototypes (Saw NP) during alien category learning: 
- `TypicalityC`: saw prototypes (.5) vs. saw non-prototypes during alien category learning 
- `Trial`: trial number in experiment [range: 9-731, only 180 total trials]
- `TrialBin`: trials binned by every 30 [range: 1-6]
- `stimulus`: which alien body was presented during that trial
- `Correct`: whether the response was correct (1) or incorrect (0) to classify the alien
- `totalCorrect`: total number of correct trials for a given participant [range: 0-180]
- `NoticedColor`: responses to color patterns questionnaire - noticers (1) vs. non-noticers (0)
- `UsedColor`: responses to strategy questionnaire - participants who reported using color (1) vs. those who did not report using colors (0) during the alien category learning task





SUPPLEMENTAL DATA: 

`GoodFit-ColorName`: stores the data for the Goodness-of-fit color-name association task. Columns include: 
- `Subj`: participant number 
- `Ratings`: Association rating averaged over two presentations of a given color for each name [range: 0-1] 
- `ColorID`: Hue of color 
- `Sat.Light`: Whether color was saturated (1), light (2), muted (3), or dark (4) according to the BCP classifications
- `Warmness`: how warm vs. cool a color was [range: 1 (cyan) to 5 (orange)]
- `WarmnessC`: how warm vs. cool a color was, mean centered [range: -2 (cyan) to 2 (orange)]
- `NameStr`: FILK or SLUB
- `NameC`: Filk (0.5) or Slub (-0.5) species


`GoodFit-ShapeColor`: stores the data for the Goodness-of-fit shape-color association task. Columns include: 
- `Subj`: participant number 
- `Ratings`: Association rating for a given color and body shape [range: 0-1] 
- `ColorID`: Hue of color 
- `BodyNum`: Which specific body shape was presented [range: 1 to 5]
- `ShapeStr`: Pointy vs. Curvy body shapes
- `ShapeC`: Pointy (-.5) vs. curvy (.5) body shapes
- `Warmness`: how warm vs. cool a color was [range: 1 (cyan) to 5 (orange)]
- `WarmnessC`: how warm vs. cool a color was, mean centered [range: -2 (cyan) to 2 (orange)]


`GoodFit-ShapeName`: stores the data for the Goodness-of-fit shape-name color association task. Columns include: 
- `Subj`: participant number 
- `Ratings`: Association rating for the given name & body shape [range: 0-1] 
- `NameC`: Filk (0.5) or Slub (-0.5) species
- `ShapeC`: Pointy (-.5) vs. curvy (.5) body shapes
- `LegType`: which of the five leg types the given alien had [range: 1 to 5]
- `BodyNum`: Which specific body shape was presented [range: 1 to 5]
- `ShapeStr`: Pointy vs. Curvy body shapes



### R scripts

`CCAssociations`: Code for LMER analyses and plotting for all experiments (Exps. 1 & 2, and Goodness-of-fit)


### Materials

`Experiment1ColorCoordinates`: Color coordinates for BCP-32 colors used in Experiment 1. 

`Experiment2ColorCoordinates`: Color coordinates for prototypical and non-prototypical colors used in Experiment 2. 

`Materials folder`: Alien stimuli


