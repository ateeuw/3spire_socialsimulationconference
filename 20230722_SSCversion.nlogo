;; 3spire 30-5-2023
;; Project for this version: Find out why and remove - if any - artefacts causing households to have very large negative incomes
;; author: Aleid Sunniva Teeuwen


;; try:

;; Improving:
;; - add business as usual as a strategy  --> works :)
;; - add change to fallow as a strategy --> works but nobody chooses it

;; Debugging:
;; - track bought cattle with "bought?" cattle characteristic --> this works :) --> actually, it might not be working (see household 26)
;; - track feed adoption with "adopted-feed?" household characteristic --> this seems to work, but check households 15 and 26
;; -- try something similar for fertiliser?
;; - add expected costs to out file, check in R
;; - print test-cowcost (and also fertiliser and feed) everywhere, put marker before so you can find out where the value has changed
;; - reduce the loop function to just one strategy (e.g. just buy feed)
;; - add "adopted-strategies" characteristic and add each strategy to this list each time step
;; - ask abm group? how do they debug their code?

;; Cleaning
;; - remove unused characteristics


extensions [nw csv table]

globals[
  forgetfullness; vector with values 0.5, 0.75, 0.9 for example <- HOW TO USE THESE IN AN ASK command? GET?
  area-field; area of one field agent
  cowiter; dummy used in loop function
  cowid; dummy used in loop function
  all-cattle; dummy list used in loop function
  gini
  sorted-weights
  selected-weights
  b-constant ; constant used in aspiration adaptation formula

  ; financial
  global-maize-price
  global-veg-price
  global-butter-price
  global-coffee-price
  global-supplementary-feed-price
  global-cow-sell-price
  global-cow-purchase-price
  global-bull-sell-price
  global-heifer-sell-price
  global-oxen-sell-price
  global-coffeeplantation-price

  ; technical
  milk-to-butter-conversion-factor
  utilisable-pasture
  utilisable-greenmaizestover
  utilisable-drymaizestover
  utilisable-vegetableresidues
  utilisable-coffeehusk
  utilisable-coffeeleaves
  feed-from-maize
  feed-from-coffee
  maize-yield-low
  maize-yield-moderate
  maize-yield-high
  maize-yield-veryhigh
  vegetable-yield-low
  vegetable-yield-moderate
  vegetable-yield-high
  vegetable-yield-veryhigh
  coffee-yield-low-establishment
  coffee-yield-moderate-establishment
  coffee-yield-high-establishment
  coffee-yield-veryhigh-establishment
  coffee-yield-low-initiation
  coffee-yield-moderate-initiation
  coffee-yield-high-initiation
  coffee-yield-veryhigh-initiation
  coffee-yield-low-full
  coffee-yield-moderate-full
  coffee-yield-high-full
  coffee-yield-veryhigh-full
  coffee-yield-low-ageing
  coffee-yield-moderate-ageing
  coffee-yield-high-ageing
  coffee-yield-veryhigh-ageing
]

breed [households household]
breed [fields field]
breed [cattle cow]
breed [members member] ; household members

undirected-link-breed [mycows mycow]
undirected-link-breed [mycalves mycalf]
undirected-link-breed [myfields myfield]
undirected-link-breed [myfarms myfarm]
undirected-link-breed [mymembers mymember]
undirected-link-breed [friendships friendship]
directed-link-breed [influencers influencer]

friendships-own[weight]

households-own [

  ;;household characteristics
  name
  household-size;
  candidate-weight; needed to form friendships
  network-size;
  children;
  elders;
  adults;
  savings;
  food-requirement;
  maize_selfconsumption
  workload

  ;; farm inputs
  fertiliser;

  ;; productive means
  labourcap; max hours a household can work per week
  tlu;
  herd;
  oxen;
  cows;
  calves;
  number_fields;
  farmland; area of agricultural land
  maizeland; area grown with maize
  vegetableland; area grown with maize
  privatepasture; area grown with pasture
  coffeeland; area grown with coffee
  fallowland; area left fallow

  ;; choices
  chosen; is yes when a strategy has been chosen, remain no otherwise
  chosen-strategy; a string with the chosen strategy
  testing-strategy; a string with the strategy a household is testing out at that moment or tested out last
  adopted-feed?; yes if feed has been adopted
  order-strategies; a list of the scores of the strategies in the order
  loop-order;
  known-strategies; a list of know strategies
  strat-dummy
  pos; to keep track of which position strategies should have in order-strategies list
  testincomeoutcomes; when households go through the loop, the expected income outcomes are stored in this list --> order is the same as the loop-order
  testfoodoutcomes; when households go through the loop, the expected food outcomes are stored in this list --> order is the same as the loop-order
  pastureincomeoutcomes
  pasturefoodoutcomes

  ;; aspirational outcomes
  maizeproduction; production of maize current year
  vegetableproduction; production of maize current year
  milkproduction; production of milk current year
  milk-consumed
  butterproduction; production of butter current year
  coffeeproduction; production of coffee current year
  coffeesales ; how much farmers earn from coffee without substracting costs
  feedproduction;
  maizeprofit; profit from maize sales current year
  maizesales; earnings from sales of maize current year
  maizepurchase; the maize farmers purchase (only if they do not grow enough themselves)
  maize-consumed
  maize-left; need it for now to work out code
  maizecost; cost of producing maize
  vegetableprofit; profit from vegetable sales current year
  vegetablesales; earnings from sales of vegetables current year
  vegetablecost; the cost of producing vegetables current year
  veg-consumed; vegetables consumed annually, kg
  butterprofit; profit from butter sales current year (supplementary feed subtracted)
  supplementaryfeedcost ; price of supplementary feed per year
  butter-consumed; the amount of butter consumed by household (kg per year)
  buttersales; earnings from butter sales without subtracting costs
  cowsales; earnings from selling cows (if this option is chosen)
  cowcost; cost of buying new cattle
  bullsales; profit from bull sales current year
  oxensales; profit from oxen sales current year
  coffeeprofit; profit from coffee sales current year
  coffeecost; cost of producing coffee current year
  coffee-consumed; coffee consumed by household per year kg
  laboursum; the sum of the labour cost of all activities
  value-of-consumed-products ; the yearly economic value of the products that households consume
  food-purchases
  costs-of-production
  earnings-from-sales

  leisure-outcome-t3; aspiration outcome three years ago
  leisure-outcome-t2; aspiration outcome two years ago
  leisure-outcome-t1start; aspiration outcome one years ago
  leisure-outcome-t1end; aspiration outcome end of time step
  leisure-outcomes-past

  food-outcome-t3; aspiration outcome three years ago
  food-outcome-t2; aspiration outcome two years ago
  food-outcome-t1start; aspiration outcome one years ago
  food-outcome-t1end; aspiration outcome end of time step
  food-outcomes-past

  income-outcome-t3; aspiration outcome three years ago
  income-outcome-t2; aspiration outcome two years ago
  income-outcome-t1start; aspiration outcome one years ago
  income-outcome-t1end; aspiration outcome end of time step
  income-outcomes-past

  ;; aspiration thresholds
  leisure-threshold-t3; thresholds three years ago
  leisure-threshold-t2; thresholds two years ago
  leisure-threshold-t1start; thresholds one years ago
  leisure-threshold-t1end; thresholds at end of this years time step
  leisure-thresholds-past

  food-threshold-t3; thresholds three years ago
  food-threshold-t2; thresholds two years ago
  food-threshold-t1start; thresholds one years ago
  food-threshold-t1end; thresholds at end of this years time step
  food-thresholds-past

  income-threshold-t3; thresholds three years ago
  income-threshold-t2; thresholds two years ago
  income-threshold-t1start; thresholds one years ago
  income-threshold-t1end; thresholds at end of this years time step
  income-thresholds-past

  leisure-threshold-mean; weighted mean of past thresholds
  food-threshold-mean; weighted mean of past thresholds
  income-threshold-mean; weighted mean of past thresholds

  food-relativeimportance; relative importance of food aspiration compared to other aspirations
  income-relativeimportance;
  leisure-relativeimportance;

  ;; expected aspirational outcomes calculated when assessing different strategies
  test-maizeproduction
  test-maizesales
  test-maizecost
  test-maizeconsume
  test-vegetableproduction
  test-vegetablesales
  test-vegetablecost
  test-feedproduction
  test-herd
  test-milkproduction
  test-butterproduction
  test-buttersales
  test-cowcost
  test-feedcost
  test-cowsales
  test-bullsales
  test-oxensales
  test-coffeeproduction
  test-coffeesales
  test-coffeecost
  leisure-expectation
  food-expectation
  income-expectation

  ; aspiration gaps
  income-gap
  food-gap
  leisure-gap
  satisficed; yes if all gaps > 0, no otherwise

  ; experience-weights
  ew-income
  ew-food
  ew-leisure

]

fields-own[
  field-size;
  current-land-use; maize, coffee, pasture, or pasture
  past-land-uses; vector with land uses of past years
  coffee-age; -1 if not coffee, otherwise age in years
  owned-by ; who-id or -1 if not owned by anyone
  yield; production per ha
  production; production in total (not per ha)
  labour;
  fertiliser; can be 0 (none), 88 (moderate), 145 (high), or 209 (very high)

  ; test characteristics for assessing expected outcomes of strategies
  test-production
  test-yield
]

cattle-own[
  manure; in kg
  age;
  castrated;
  value;
  feed-quantity;
  supplementary-feed; yes or no
  pasture-quantity;
  productivity;
  milkyield;
  pregnant; yes or no
  sex; male or female
  in-lactation; yes or no
  lactation; the number of lactations the cow has had
  dry;
  calf; the number of calves the cow has had
  bought? ; will be true if the cattle were purchased and false if the cattle owned at the beginning of the simulation or born
]

patches-own [
  ; soil
  ; climate
  farmstead ; 1 if there is a household on it, 0 otherwise
  afield ; 1 if there is a field on it, 0 otherwise
]

members-own[lifestage]

;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all
  file-close-all
  reset-ticks

  if fix-seed? [ random-seed behaviorspace-run-number ] ; random seed will be 0 by default, but if we run in behaviorspace, the seed for the first run will be 1, then 2, then 3, etc

  ; test if the filled in global values are OK
  test-lu

  print ""
  print word "NEW RUN " date-and-time
  print word "Aspiration adaptation formula: " Aspiration-adaptation
  print "=============================================================================="

  ;; initialisation

  ; prices (global)
  set global-maize-price 10                                       ; average domestic maize price per kg over the last years (2009-2018) was 4.8 birr (Minten 2020b, p. 290), but the since the conflict and the drought it has been reported being >20 birr at wholesale (http://www.egte-ethiopia.com/en/in-t-market/commodity-statistics.html)
  set global-veg-price 30 ;
  set global-butter-price 200 ;
  set global-coffee-price 140                                     ;according to Diro 2019 ; parameter
  set global-supplementary-feed-price 142.92 * 12                 ; price of buying supplementary feed
  set global-cow-sell-price 5286                                  ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 600 = what farmers are likely to get for selling cow at the local market (= lower end of price range), source: Anteneh 2010 ; parameter (seems quite cheap, adjust for inflation?)
  set global-cow-purchase-price 6090                              ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 1600 = cost of buying a cow at the local market (= higher end of price range), source: Anteneh 2010 ; parameter (seems quite cheap, adjust for inflation?)
  set global-bull-sell-price 8376                                 ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 6000 but I am not sure where I got that number from
  set global-heifer-sell-price 3510                               ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R
  set global-oxen-sell-price 8730                                 ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 10000 but I am not sure where I got that number from
  set global-coffeeplantation-price 79921                         ; etb per ha --> reference = Diro 2019 ; parameter

  ; other (global)
  set milk-to-butter-conversion-factor 16.5                       ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week. ; About 16.5 litres of milk is required to produce a kilogram of butter (Anteneh 2010, p. 23)
  set utilisable-pasture 0.75 * 2                                  ; see sources on slide 20 C:\Users\TeeuwenAS\OneDrive - Universiteit Twente\Twente\Thesis shared - ABM value chains food security\Meetings\20221011_Supervision meeting
  set utilisable-greenmaizestover 0.91 * 0.91
  set utilisable-drymaizestover 1.5 * 0.93
  set utilisable-vegetableresidues 0
  set utilisable-coffeehusk 0.3 * 3
  set utilisable-coffeeleaves 1 * 0.18
  set maize-yield-low 2.718 * 1000                                ; yield units = kg per ha. Source maize yields: van Dijk 2020 & Assefa 2021
  set maize-yield-moderate 3.311 * 1000
  set maize-yield-high 4.356 * 1000
  set maize-yield-veryhigh 4.664 * 1000
  set vegetable-yield-low 14.9 * 1000                             ; Look back what the source for these were, see ppt presentation C:\Users\TeeuwenAS\OneDrive - Universiteit Twente\Supervision meeting add fertiliser
  set vegetable-yield-moderate 38.2 * 1000
  set vegetable-yield-high 41.9 * 1000
  set vegetable-yield-veryhigh 56 * 1000
  set coffee-yield-low-establishment 50.34                        ; Look back what the source for these were, see ppt presentation C:\Users\TeeuwenAS\OneDrive - Universiteit Twente\Supervision meeting add fertiliser
  set coffee-yield-moderate-establishment 98
  set coffee-yield-high-establishment 84
  set coffee-yield-veryhigh-establishment 142.2
  set coffee-yield-low-initiation 654.36
  set coffee-yield-moderate-initiation 1218
  set coffee-yield-high-initiation 1512
  set coffee-yield-veryhigh-initiation 1564
  set coffee-yield-low-full 906.04
  set coffee-yield-moderate-full 1694
  set coffee-yield-high-full 2016
  set coffee-yield-veryhigh-full 1864.44
  set coffee-yield-low-ageing 453.02
  set coffee-yield-moderate-ageing 1260
  set coffee-yield-high-ageing 1512
  set coffee-yield-veryhigh-ageing 1121.08

  ; read households with household characteristics from csv
  read-households-from-csv

  ask households[
    set pcolor grey
    ask patch-here [
      sprout-cattle round [tlu] of myself
      sprout-fields [number_fields] of myself
      sprout-members [household-size] of myself
      set farmstead 1
    ]
  ]

  ; set global variables
  set forgetfullness (list remember-t3 remember-t2 remember-t1)
  set area-field 1
  set b-constant (ifelse-value
    Aspiration-adaptation = "no-experience" [b-no]                ; 𝐴𝐿_(𝑎 𝑡)=𝐴𝐿_(𝑎 𝑡−1) ∗ 𝑜𝑝𝑡𝑖𝑚𝑖𝑠𝑚   − 𝑏_𝑛𝑜𝑒𝑤∗𝐴𝐺_(𝑎 𝑡−1)
    Aspiration-adaptation = "no-experience-alternative" [b-no]    ; 𝑖𝑓(𝐴𝐿_(𝑎 𝑡−1)  ≥𝐴𝑂_(𝑎 𝑡−1))  [𝐴𝐿_(𝑎 𝑡)=𝐴𝐿_(𝑎 𝑡−1)∗𝑜𝑝𝑡𝑖𝑚𝑖𝑠𝑚−𝑏_𝑛𝑜∗𝐴𝐺_(𝑎 𝑡−1)]  𝑒𝑙𝑠𝑒  [𝐴𝐿_(𝑎 𝑡)=𝐴𝑂_(𝑎 𝑡−1)∗𝑜𝑝𝑡𝑖𝑚𝑖𝑠𝑚+𝑏_𝑛𝑜∗𝐴𝐺_(𝑎 𝑡−1)]
    Aspiration-adaptation = "change-rate-experience" [b-rate]     ; 𝐴𝐿_(𝑎 𝑡)=𝐴𝐿_(𝑎 𝑡−1) ∗ 𝑜𝑝𝑡𝑖𝑚𝑖𝑠𝑚   − 𝑏_𝑛𝑜𝑒𝑤∗𝐴𝐺_(𝑎 𝑡−1)  + 𝑏_𝑟𝑎𝑡𝑒   ∗ 𝐴𝐺_(𝑎 𝑡−1)  * 𝑐ℎ𝑎𝑛𝑔𝑒 𝑟𝑎𝑡𝑒
    Aspiration-adaptation = "log-experience" [b-log]              ; 𝐴𝐿_(𝑎 𝑡)=𝐴𝐿_(𝑎 𝑡−1) ∗ 𝑜𝑝𝑡𝑖𝑚𝑖𝑠𝑚   − 𝑏_𝑛𝑜𝑒𝑤∗𝐴𝐺_(𝑎 𝑡−1)  + 𝑏_𝑙𝑜𝑔𝑒𝑤  ∗ 𝐴𝐺_(𝑎 𝑡−1)  ∗ 𝑙𝑜𝑔𝑒𝑤_𝑎
    Aspiration-adaptation = "linear-experience" [b-lin]           ; 𝐴𝐿_(𝑎 𝑡)=𝐴𝐿_(𝑎 𝑡−1) ∗ 𝑜𝑝𝑡𝑖𝑚𝑖𝑠𝑚   − 𝑏_𝑛𝑜𝑒𝑤∗𝐴𝐺_(𝑎 𝑡−1)  + 𝑏_𝑒𝑤    ∗ 𝐴𝐺_(𝑎 𝑡−1)   ∗ 𝑒𝑤_𝑎
  )

  ; attribute cattle to households
  assign-cattle

  ; position fields and attribute to households

  ask households [ask fields-here[set field-size [farmland] of myself / [number_fields] of myself]] ; assuming all fields are equally large

  ask fields [
    set shape "square"
    set color white
    set pcolor grey
    create-myfields-with other households-here
    forward 1
    position-fields
    set fertiliser "low"
  ]

  let n-fields count fields
  ask n-of floor (n-fields * chance-maize) fields [ set color yellow set current-land-use "maize"] ; maize fields
  ask n-of floor ((n-fields * chance-pasture) - 1) fields with [color != yellow] [ set color green set current-land-use "pasture"] ; pasture fields
  ask n-of floor ((n-fields * chance-veg) - 1) fields with [color != yellow and color != green] [ set color orange set current-land-use "vegetable"] ; pasture fields
  ask n-of floor ((n-fields * chance-coffee) - 1) fields with [color != yellow and color != green and color != orange] [ set color brown set current-land-use "coffee" set coffee-age random 29 + 1] ; pasture fields
  ask fields with [color != yellow and color != green and color != orange and color != brown] [ set color white set current-land-use "fallow"] ; coffee fields

  ; set household variables
  ask households [
    ;; characteristics
    set savings 0
    set farmland count in-myfield-neighbors ;* area-field
    set maizeland count in-myfield-neighbors with [current-land-use = "maize"] * area-field
    set vegetableland count in-myfield-neighbors with [current-land-use = "vegetable"] * area-field
    set privatepasture count in-myfield-neighbors with [current-land-use = "pasture"] * area-field
    set coffeeland count in-myfield-neighbors with [current-land-use = "coffee"] * area-field
    set fallowland count in-myfield-neighbors with [current-land-use = "fallow"] * area-field
    set herd count in-mycow-neighbors
    set oxen count in-mycow-neighbors with [sex = "male" and castrated = "yes"]

    ;; assign memory to households
    set known-strategies [] ; empty list
    set order-strategies [0 0 0 0 0 0 0 0 0 0]; the number of zeros should equal the number of strategy options
    set strat-dummy 0
  ]

  ask households [set known-strategies fput "stick to current" known-strategies]
  ask households [set known-strategies fput "abandon land" known-strategies]
  ask households with [maizeland > 0][set known-strategies fput "change to maize" known-strategies]
  ask households with [vegetableland > 0][set known-strategies fput "change to vegetable" known-strategies]
  ask households with [privatepasture > 0][set known-strategies fput "change to pasture" known-strategies]
  ask households with [herd > 0 and privatepasture <= 0][set known-strategies fput "change to pasture" known-strategies]
  ask households with [coffeeland > 0][set known-strategies fput "plant coffee" known-strategies]
  ask households with [herd > 0][set known-strategies fput "buy cow" known-strategies]
  ask households with [herd > 0][set known-strategies fput "sell cow" known-strategies]
  ask n-of (round Number-of-households * 0.27) households with [herd > 0][set known-strategies fput "buy feed" known-strategies] ; parameter here = 0.27
  ask n-of (round Number-of-households * 0.80) households [set known-strategies fput "buy fertiliser" known-strategies] ; parameter here = 0.80, just a wild guess for now, do research!
  ask households [set labourcap adults * 365] ; person days per year


  ask households [
    ;; assign baseline aspiration values to households
    assign-aspiration-outcomes
    assign-aspiration-thresholds
    assign-relativeimportance
  ]

  ; attribute household members to households
  assign-members
  ask members[forward 1]

  calculate-gini ; is done in setup and in go procedure

  ; load social network
  read-matrix
  select-friends

  ; write to file
  if write-file? [
    let file (word "Netlogo_output/files_written_auto/" Aspiration-adaptation "_" Strategy-order "_" b-no "_" b-constant "_" optimism ".txt")

    if is-string? file [
      if file-exists? file [
        file-delete file]
      file-open file
      write-to-file
    ]
  ]

  reset-ticks
end

;; RUN SIMULATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  print word "year:" ticks
  ask households [

    ; STEP 1: update aspirations
    update-aspiration-outcomes                                             ; Update aspiration outcomes
    update-aspiration-thresholds                                           ; Update aspiration thresholds
    calculate-new-aspiration-thresholds                                    ; Calculate new aspiration thresholds

  ]

  ; STEP 2: Determine order of livelihood improvement strategies
  ask households [                                                         ; Start with clean slate; households have not yet chosen a strategy
    set chosen "no"
    set chosen-strategy 0
    set color white
  ]

  ; Determine which order households should loop through strategies in. They will consider the strategies that are most similar to their current management first. Strategies unknown to households will not be considered. If two or more strategies are equally similar, they will be ordered randomly.
  determine-loop-order

  ask households with [satisficed = "no"][
    foreach loop-order [                                                   ; Remove strategies that the farmers do not know
      x -> if member? x known-strategies = false [set loop-order remove-item (position x loop-order) loop-order]
    ]
  ]

  ; All households that are not yet satisficed will loop through strategies till they find a satisficing solution.

  ask households [if who = test-hh  [print (word "is household " who " satisficed? " satisficed)]]

  ask households with [satisficed = "yes"][
    set-testing-vars-na
  ]

  ask households with [satisficed = "no"][

    ; if who = test-hh  [print "have entered the big for loop, will now loop through strategies first time"]

    loop-through-strategies
    ifelse chosen-strategy = 0 [set color red][set color green - 2]        ; Households that find a satisficing solution are turned light green. Unsatisficed households are turned red.
    set cowiter 0

    let aspiration-thresholds-before-loop
            (list (income-threshold-mean)                                  ; Store pre-downadjustment aspiration thresholds in list
                  (food-threshold-mean) (leisure-threshold-mean))

    ; While households could not find a satisficing strategy, they will look for close-to-satificing strategies. This is realised by gradually lowering their aspiration thresholds.
    while [color = red and chosen-strategy = 0 and                         ; Have not found a strategy
           cowiter < 20 and                                                ; They will lower 20 times, after that, they go to more drastic means
           (income-threshold-mean != 0 or                                  ; Thresholds cannot be lowered below 0
            food-threshold-mean != 0 or
            leisure-threshold-mean != 0)][

      set income-threshold-mean income-threshold-mean -                    ; How much the thresholds are lowered, depends on the relative importance of the aspirational dimension
          (item 0 aspiration-thresholds-before-loop * ( income-relativeimportance / 100) * 0.1 )
      set food-threshold-mean food-threshold-mean -
          (item 1 aspiration-thresholds-before-loop * ( food-relativeimportance / 100) * 0.1 )
      set leisure-threshold-mean leisure-threshold-mean -
          (item 2 aspiration-thresholds-before-loop * ( leisure-relativeimportance / 100) * 0.1 )

      ; if who = test-hh  [print word "looping through strategies in while loop, iteration = " cowiter]

      loop-through-strategies
      ifelse chosen-strategy = 0 [set color red][set color green - 2] ;;her
      set cowiter cowiter + 1
    ]

    ; While households could not find a close-to-satisficing strategy by down-adjusting their aspiration thresholds, do something more drastic: drop one or several aspiration thresholds
    let cow-id 0
    set income-threshold-mean item 0 aspiration-thresholds-before-loop
    set food-threshold-mean item 1 aspiration-thresholds-before-loop
    set leisure-threshold-mean item 2 aspiration-thresholds-before-loop

    while [color = red and cow-id < 6] [
      set cow-id cow-id + 1
      if cow-id = 1 [
        let aspiration-thresholds-dropped drop-least-important             ; Drop the least important aspiration dimension (1 1 0)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 2 [
        let aspiration-thresholds-dropped drop-2ndleast-important          ; Drop the 2nd least important aspiration dimension (1 0 1)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 3 [
        let aspiration-thresholds-dropped keep-only-most-important         ; Drop the two least important aspiration dimensions (1 0 0)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 4 [
        let aspiration-thresholds-dropped drop-most-important              ; Drop the most important aspiration dimension (0 1 1)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 5 [
        let aspiration-thresholds-dropped keep-only-2ndmost-important      ; Drop most and least important aspiration dimensions (0 1 0)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 6 [
        let aspiration-thresholds-dropped keep-only-least-important        ; Drop 2nd least and least important aspiration dimensions (0 0 1)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      ; if who = test-hh  [print word "looping through strategies in while loop where aspiration dimensions are dropped, iteration = " cow-id]

      loop-through-strategies
      ; if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || right after loop where dimensions are dropped")]
      ifelse chosen-strategy = 0 [set color red][set color green - 2]

    ]

    ; if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || right before resetting thresholds")]

    set income-threshold-mean item 0 aspiration-thresholds-before-loop
    set food-threshold-mean item 1 aspiration-thresholds-before-loop
    set leisure-threshold-mean item 2 aspiration-thresholds-before-loop

    ; if who = test-hh [print (word "hh " who " test-cowsales: " test-cowsales " || right after resetting thresholds")]      ;; here it is 3510

    ; this is just a test to see what happens
    set cowsales test-cowsales
    ; if who = test-hh [print (word "** hh " who " cowsales: " cowsales " || right after resetting thresholds")]  ; 3510
    ; this is just a test to see what happens

  ]

  ; ask households with [who = test-hh] [ print "" print (word "hh with " who " test-cowsales: " test-cowsales " || right after the big for loop, before resetting cattle")] ; zero

  ; ask households [if who = test-hh [
  ;  print (word "hh " who " test-cowsales: " test-cowsales " || right after the big for loop, before resetting cattle")    ;; here it is zero --> WHY?
  ;  print (word "** hh " who " cowsales: " cowsales " || right after the big for loop, before resetting cattle")            ; 3510
  ;  ]
  ;]

  ask cattle with [color != orange] [
    set color ifelse-value (sex = "female")[pink + 2][sky + 2]]            ; Restore color changed when looping through and testing different strategies

  ; STEP 5: Perform strategies
  ;ask households [if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || before perform-chosen-management")] ]
  ; ask households [if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || before perform-chosen-management")] ]

  perform-chosen-management                                                ; Implement the baseline management and (if any) strategies selected in the previous step
  ;ask households [if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || after perform-chosen-management")] ]
  ; ask households [if who = test-hh [print (word "hh " who " test-cowsales: " test-cowsales " || after perform-chosen-management")] ]

  ; update characteristics
  update-hh-characteristics                                                ; Land use and livestock
  calculate-aspirational-outcomes                                          ; Leisure, income & and other financial indicators, and food self-sufficiency
  ;ask households [if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || after updating hh characteristics and aspirational outcomes")] ]
  ; ask households [if who = test-hh [print (word "hh " who " test-cowsales: " test-cowsales " || after updating hh characteristics and aspirational outcomes")] ]

  ; STEP 6: Simulate the transfusion of knowledge through farmers social networks
  ask households with [chosen-strategy != 0] [                             ; If households did not choose a new strategy, there is no new knowledge to share
    ask out-influencer-neighbors [                                         ; Out-influencer-neighbors of x are households that are influenced by a household x
      ifelse member? chosen-strategy known-strategies                      ; Check of the neighbors already know the strategy that the household chose
      [set known-strategies known-strategies]                              ; No change
      [set known-strategies lput chosen-strategy known-strategies]         ; New strategy is added to neighbors knowledge
    ]
  ]

  calculate-gini                                                           ; Calculate population level inequality (gini-index). This does not work because incomes can be negative. Need to find a different indicator or adapt it to allow for negative incomes

  print (word "end of year " ticks)
  tick
  if write-file? [write-to-file]                                           ; Record model and household characteristics each year and write to a csv file.
  ; ask households [if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || end of go")] ]
  ; ask households [if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || end of go")] ]

end

;; FUNCTIONS used in setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to read-households-from-csv
  file-close-all                                                           ; Close all open files

  if not file-exists? "Netlogo_Input/netlogopop.csv" [
    user-message "Netlogo_Input/netlogopop.csv' exists! Try pressing WRITE-HHS-TO-CSV."
    stop
  ]

  file-open "Netlogo_Input/netlogopop.csv" ; open the file with the turtle data

  while [ not file-at-end? ] [                                             ; We'll read all the data in a single loop
    let data csv:from-row file-read-line                                   ; here the CSV extension grabs a single line and puts the read data in a list
                                                                           ; now we can use that list to create a turtle with the saved properties
    create-households 1 [
      set name item 0 data
      set xcor    item 1 data
      set ycor    item 2 data
      set size    item 3 data
      set color   item 4 data
      set heading item 5 data
      set shape   item 6 data
      set household-size item 7 data
      set children item 8 data
      set elders item 9 data
      set adults item 10 data
      set tlu item 11 data
      set oxen item 12 data
      set cows item 13 data
      set calves item 14 data
      set farmland item 15 data
      set number_fields item 16 data
      set food-outcome-t1end item 17 data                                ; = maize consumption
      set maize_selfconsumption item 18 data
      set workload item 19 data                                          ; this is in hours per household per week
      set network-size item 20 data
    ]
  ]

  file-close                                                             ; make sure to close the file
end

to assign-cattle

  ask households [
    let perc_cows ifelse-value (calves + oxen + cows) > 0 [               ; Determine how many percentage of the cattle should be cows --> parameter perc_cows
      cows / (calves + oxen + cows)][0]
    let perc_oxen ifelse-value (calves + oxen + cows) > 0 [               ; Determine how many percentage of the cattle should be oxen --> parameter perc_cows
      oxen / (calves + oxen + cows)][0]

    ask cattle-here [                                                     ; Assign default generic cattle characteristics
      set shape "cow"
      set dry "yes"
      set castrated "no"
      set supplementary-feed "no"
      set bought? "no"
      create-mycows-with other households-here                            ; Link cattle to household owning them
    ]

    ask n-of floor (perc_cows * tlu) cattle-here [                        ; Assign characteristics to cows
      set sex "female"
      set age (random 12 + 3)                                             ; --> parameter age
      set color pink + 2
      forward 3
    ]

    ask n-of floor (perc_oxen * tlu) cattle-here [                        ; Assign characteristics to oxen
      set sex "male"
      set castrated "yes"
      set age random 7 + 1                                                ; Male cattle are only kept till they are maximum 7 years old. After that they are no longer useful; eaten or sold
      set color sky + 2
      forward 3
    ]

    ask cattle-here[                                                      ; The remaining cattle are calves. Assign them calf characteristics
      set sex item random 2 ["female" "male"]
      set age random 3
      ifelse sex = "female" [set color pink + 2] [set color sky + 2]
      forward 3
    ]
  ]

  ask n-of (count cattle with [                                           ; Cows are pregnant approximately every other year between the age of 3 and 11
    sex = "female" and age < 11 and age > 2] / 2) cattle with [sex = "female" and age < 11 and age > 2] [set pregnant "yes"]
  ask cattle with [(age = 4 or age = 5) and pregnant != "yes" and         ; After pregnancy, they give birth and then lactate. Cows generally do not birth more than four calves
    sex = "female"][set lactation 1 set in-lactation "yes" set calf 1]
  ask cattle with [(age = 6 or age = 7) and pregnant != "yes" and sex = "female"][set lactation 2 set in-lactation "yes"  set calf 2]
  ask cattle with [(age = 8 or age = 9) and pregnant != "yes" and sex = "female"][set lactation 3 set in-lactation "yes"  set calf 3]
  ask cattle with [(age = 10 or age = 11) and pregnant != "yes" and sex = "female"][set lactation 4 set in-lactation "yes"  set calf 4]
  ask cattle [set size age / 8 + 1]

end

to position-fields ; assumption
  while [any? other fields-here or any? other households-here]            ; This gives a nice layout where fields do not overlap and are placed randomly in the proximity of the household owning them
  [ fd 1 ]
  setxy round xcor round ycor                                             ; The center of fields is put in the center of the patch underneath, gives a tidy look
end

to assign-aspiration-outcomes
    calculate-expected-outcome
    set income-outcome-t1end income-expectation                           ; We did not have data about initial income from the survey data, so used the other data (land use and herd size and composition) to calculate their expected income
    set income-outcome-t1start income-outcome-t1end
    set income-outcome-t2 income-outcome-t1start
    set income-outcome-t3 income-outcome-t2

    set food-outcome-t1start food-outcome-t1end                           ; Food self-sufficiency outcome is based on survey data
    set food-outcome-t2 food-outcome-t1start
    set food-outcome-t3 food-outcome-t2

    let workcap_persondays adults * 7                                     ; Number of adults is based on survey data --> parameter worcap_persondays
    let workload_persondays workload / 8                                  ; Workload is given in hours per household per week in the survey, transform to person days per week by dividing by 8 hours per day
    set leisure-outcome-t1end workcap_persondays - workload_persondays    ; Leisure days per household per week
    set leisure-outcome-t1start leisure-outcome-t1end
    set leisure-outcome-t2 leisure-outcome-t1start
    set leisure-outcome-t3 leisure-outcome-t2
end

to assign-aspiration-thresholds
  set income-threshold-t1end income-outcome-t1end +
                             random-exponential 0.86 * 100000             ; Fitted based on the data of Daniel Mekonnen (see Initial_aspiration.ppt + current_vs_aspired_surveydata.R)
  if income-threshold-t1end <= 0 [set income-threshold-t1end 0.01]
  set income-threshold-t1start income-threshold-t1end
  set income-threshold-t2 income-threshold-t1start
  set income-threshold-t3 income-threshold-t2

   set food-threshold-t1end food-outcome-t1end +
                            random-normal 6.91 26.99                      ; Fitted based on data of Ermias Tesfaye (see Initial_aspiration.ppt + current_vs_aspired_surveydata.R)
  if food-threshold-t1end <= 0 [set food-threshold-t1end 0.01]
  set food-threshold-t1start food-threshold-t1end
  set food-threshold-t2 food-threshold-t1start
  set food-threshold-t3 food-threshold-t2

   set leisure-threshold-t1end leisure-outcome-t1end +
                              random-normal 0.98 1.68                     ; Fitted based on data of Ermias Tesfaye (see Initial_aspiration.ppt + current_vs_aspired_surveydata.R)
  if leisure-threshold-t1end <= 0 [set leisure-threshold-t1end 0.01]
  set leisure-threshold-t1start leisure-threshold-t1end
  set leisure-threshold-t2 leisure-threshold-t1start
  set leisure-threshold-t3 leisure-threshold-t2
end

to assign-relativeimportance                                              ; To do: base this on the data of Ermias Tesfaye
  let three-random (list (random 100 + 1) (random 100 + 1) (random 100 + 1))
  let three-sum sum three-random
  set food-relativeimportance ((item 0 three-random) / three-sum) * 100 ; parameter
  set income-relativeimportance ((item 1 three-random) / three-sum) * 100 ; parameter
  set leisure-relativeimportance ((item 2 three-random) / three-sum) * 100 ; parameter
end

to assign-members
  ask households [                                                        ; Assign members to households, and give them characteristics
    ask members-here [
      set shape "person"
      create-mymembers-with other households-here
      set color brown
      set size 0.5
    ]

    ask n-of adults members-here [set lifestage "adult"]                  ; Number of adults, children and elders are based on survey data
    ask n-of children members-here [set lifestage "child"]
    ask n-of elders members-here [set lifestage "elderly"]
  ]
end

to read-matrix
  file-close-all                                                          ; Close all open files

  if not file-exists? "Netlogo_Input/simmatrix.csv" [                     ; This is a matrix which juxtaposes all households and gives their similarity (based on household characteristics) a score
    user-message "Netlogo_Input/simmatrix.csv' exists!"
    stop
  ]

  let data csv:from-file "Netlogo_Input/simmatrix.csv"
  foreach range 100 [                                                     ; Then in a foreach loop, the friendship between those households is given a weight equal to the similarity score
    x -> let connections item x data
    ask households with [name = x][
      let other-names remove-item x range 100;
      foreach other-names[
        y -> let connection item y connections
        create-friendships-with other households with [name = y][
          set weight connection
          set hidden? true
        ]
      ]
    ]
  ]
end

to select-friends
  foreach range 100 [                                                     ; Then let households select n best friends where n = network-size, which we have from survey data
    x ->
    ask households with [name = x][
      let candidates other households with [count in-influencer-neighbors < network-size]
      ask candidates [set candidate-weight [weight] of out-friendship-to myself]

     set sorted-weights reverse sort [candidate-weight] of candidates
     set selected-weights take network-size sorted-weights

     ask candidates[if member? ([weight] of out-friendship-to myself) selected-weights [
        create-influencer-to myself [set color sky - 3]]
      ]
    ]
  ]
end

;; FUNCTIONS used in go procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Main functions
to update-aspiration-outcomes                                            ; No sub-functions
    set income-outcome-t3 income-outcome-t2
    set income-outcome-t2 income-outcome-t1start
    set income-outcome-t1start income-outcome-t1end

    set food-outcome-t3 food-outcome-t2
    set food-outcome-t2 food-outcome-t1start
    set food-outcome-t1start food-outcome-t1end

    set leisure-outcome-t3 leisure-outcome-t2
    set leisure-outcome-t2 leisure-outcome-t1start
    set leisure-outcome-t1start leisure-outcome-t1end
end

to update-aspiration-thresholds                                          ; No sub-functions
    set income-threshold-t3 income-threshold-t2
    set income-threshold-t2 income-threshold-t1start
    set income-threshold-t1start income-threshold-t1end
    set income-thresholds-past (list (income-threshold-t3) (income-threshold-t2) (income-threshold-t1end))

    set food-threshold-t3 food-threshold-t2
    set food-threshold-t2 food-threshold-t1start ;
    set food-threshold-t1start food-threshold-t1end ;
    set food-thresholds-past (list food-threshold-t3 food-threshold-t2 food-threshold-t1end)

    set leisure-threshold-t3 leisure-threshold-t2
    set leisure-threshold-t2 leisure-threshold-t1start
    set leisure-threshold-t1start leisure-threshold-t1end
    set leisure-thresholds-past (list leisure-threshold-t3 leisure-threshold-t2 leisure-threshold-t1end)
end

to calculate-new-aspiration-thresholds                                   ; No sub-functions
  ; calculate aspiration means first
  let my-forgetfullness-sum sum forgetfullness
  let my-memory-time memory-time
  let my-forgetfullness forgetfullness

  let temp1  (1 / my-forgetfullness-sum )

  let temp2 (map * my-forgetfullness income-thresholds-past)
  set income-threshold-mean sum map [ i -> temp1 * i ] temp2

  let temp4 (map * my-forgetfullness food-thresholds-past)
  set food-threshold-mean sum map [ i -> temp1 * i ] temp4

  let temp3 (map * my-forgetfullness leisure-thresholds-past)
  set leisure-threshold-mean sum map [i -> temp1 * i] temp3

  ; income
  set ew-income calculate-ew income-outcome-t3 income-outcome-t2 income-outcome-t1start Aspiration-adaptation ; depending on aspiration-adaptation choice, this will be change rate, logew, or ew
  set income-gap income-threshold-mean - income-outcome-t1start
  set income-threshold-t1end income-threshold-t1start * optimism - b-no * income-gap + b-constant * income-gap * ew-income ; if "no-experience, then ew-income = 0"
  if Aspiration-adaptation = "no-experience-alternative" [set income-threshold-t1end income-outcome-t1start * optimism + b-no * income-gap + b-constant * income-gap * ew-income]
  if income-threshold-t1end < 0 [set income-threshold-t1end 0.01]

  ; food
  set ew-food calculate-ew food-outcome-t3 food-outcome-t2 food-outcome-t1start Aspiration-adaptation ; depending on aspiration-adaptation choice, this will be change rate, logew, or ew
  set food-gap food-threshold-mean - food-outcome-t1start
  set food-threshold-t1end food-threshold-t1start * optimism - b-no * food-gap + b-constant * food-gap * ew-food ; adapt this one
  if Aspiration-adaptation = "no-experience-alternative" [set food-threshold-t1end food-outcome-t1start * optimism + b-no * food-gap + b-constant * food-gap * ew-food]
  if food-threshold-t1end > (1 * (365 / 56) * household-size) [set food-threshold-t1end (1 * (365 / 56) * household-size)] ; this is 3600 calories per day per person ( a lot! )

  ; leisure
  set ew-leisure calculate-ew leisure-outcome-t3 leisure-outcome-t2 leisure-outcome-t1start Aspiration-adaptation ; depending on aspiration-adaptation choice, this will be change rate, logew, or ew
  set leisure-gap leisure-threshold-mean - leisure-outcome-t1start
  set leisure-threshold-t1end leisure-threshold-t1start * optimism - b-no * leisure-gap + b-constant * leisure-gap * ew-leisure ;
  if Aspiration-adaptation = "no-experience-alternative" [set leisure-threshold-t1end leisure-outcome-t1start * optimism + b-no * leisure-gap + b-constant * leisure-gap * ew-leisure]
  if leisure-threshold-t1end < 0 [set leisure-threshold-t1end 0.01]
  if leisure-threshold-t1end > (5 * household-size) [set leisure-threshold-t1end 7 * household-size] ; assume max desired leisure is 5 person days per person, and that more leisure would not be desired but underemployment

  ifelse (income-gap > 0) or (food-gap > 0) or (leisure-gap > 0)[set satisficed "no"][set satisficed "yes"]

end

to determine-loop-order                                                  ; No sub-functions
    ask households with [satisficed = "no"][
    let strategy-options ["stick to current" "abandon land" "change to pasture" "change to maize" "change to vegetables" "plant coffee" "buy cow" "buy feed" "buy fertiliser" "sell cow"]
    let similarity-of-sticktocurrent     [2  2  2  2  2  2  2  2  2  2]
    let similarity-of-abandonland        [2  2  2  1  0 -2 -1 -1 -2  1]
    let similarity-of-changetopasture    [2  2  2  0 -2 -2  2  1 -2  0]
    let similarity-of-changetomaize      [2  1 -1  2 -1 -2  1  1 -1  0]
    let similarity-of-changetovegetables [2  0 -2 -1  2 -1 -1 -1 -1  0]
    let similarity-of-changetocoffee     [2 -2 -2 -2 -1  2  1 -1  0  0]
    let similarity-of-buycow             [2 -1  2  1 -2  1  2  2  1 -2]
    let similarity-of-buyfeed            [2 -1  1  1 -1  0  2  2  2 -2]
    let similarity-of-buyfertiliser      [2 -2 -2 -1 -1  0  1  2  2  0]
    let similarity-of-sellcow            [2  1  0  0  0  0 -2 -2  0  2]

     if member? "stick to current" known-strategies [
      set order-strategies (map + order-strategies similarity-of-sticktocurrent) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "abandon land" known-strategies [
      set order-strategies (map + order-strategies similarity-of-abandonland) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "change to pasture" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetopasture) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "change to maize" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetomaize) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "change to vegetables" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetovegetables) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "plant coffee" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetovegetables) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy cow" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buycow) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy feed" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buyfeed) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy fertiliser" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buyfertiliser) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "sell cow" known-strategies [
      set order-strategies (map + order-strategies similarity-of-sellcow) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    ; if who = test-hh [print (word "order options: " order-strategies)]
    let randomvalues (list (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4))
    let distinct-order  (map + order-strategies randomvalues)
    ; if who = test-hh [print (word "order options distinctly: " distinct-order)]

    let sorted-order reverse sort distinct-order
    ; if who = test-hh [print (word "sorted order: " sorted-order)]

    set loop-order []
    foreach sorted-order [
      x -> let position-strat position x distinct-order
      set loop-order lput (item position-strat strategy-options) loop-order
    ]
  ]
end

to set-testing-vars-na
  set test-feedcost "NA"
  set test-cowcost "NA"
  set test-maizecost "NA"
  set test-vegetablecost "NA"
  set test-coffeecost "NA"
  set test-maizesales "NA"
  set test-vegetablesales "NA"
  set test-coffeesales "NA"
  set test-oxensales "NA"
  set test-bullsales "NA"
  set test-cowsales 0
  set test-herd "NA"
  set income-expectation "NA"
  set leisure-expectation "NA"
  set food-expectation "NA"

end

to loop-through-strategies                                               ; Sub-functions: calculate-expected-outcome, change-from-x-to-x-test, buy-cow-test, buy-feed-test, buy-fertiliser-test

  ; if who = test-hh [print "_______________________________________________________________"]

   foreach loop-order [                                                  ; loop through farm management options and check if they will fullfill aspirations

          x ->                                                           ; it will run this loop for each x in the list loop-order. loop-order is a sorted list with all the options available to households. the length of the list differs per household.
                                                                         ; a household may have chosen a strategy before it has gone through the whole list. in this case, it will still finish the loop, but not choose anything because a condition for
                                                                         ; choosing is that chosen-strategy = 0.
    ; reset (testing things)
    set test-herd herd  ; this is needed for when cattle strategy is not chosen
    ;set test-cowsales 0
    set testing-strategy ""
    if chosen-strategy = 0 [calculate-expected-outcome]

    ; STICK TO CURRENT?
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if (x = "stick to current" and chosen-strategy = 0)[
      set testing-strategy "stick to current"
                                                                         ; No conditions need to be met to continue with current management, and no test conditions need to be changed
      calculate-expected-outcome                                         ; Calculate expected outcomes resulting from the current management

      if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
      if who = test-hh [print (word "expectations of sticking to current, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
      if who = test-hh [print ""]

      ifelse income-expectation >= income-threshold-mean and             ; Check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
        set chosen-strategy "stick to current"                           ; If they do, implement current management
      ][
        ; do nothing                                                     ; Else, do nothing
      ]


    ]

    ; ABANDON LAND?                                                      ; Check for consecutive land uses whether it is worthwhile to abandon, starting with pasture, maize, vegetables and then coffee.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if (x = "abandon land" and chosen-strategy = 0)[
      set testing-strategy "abandon land"

      if (fallowland <= farmland) [                                      ; If all land has been abandoned already, it's not possible to abandom more

        if (chosen-strategy = 0 and privatepasture > 0)[                 ; First, check if it is possible to change from PASTURE to FALLOW?????????????????????
          change-from-pasture-to-fallow-test                             ; Change a pasture into fallow
          calculate-expected-outcome                                     ; Calculate expected outcome resulting from the current manamgeent + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of abandoning land, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

          ifelse income-expectation >= income-threshold-mean and         ; Check if expected outcomes fulfill aspirations
                 food-expectation >= food-threshold-mean and
                 leisure-expectation >= leisure-threshold-mean[
            set chosen-strategy "abandon land"                           ; If they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color white]
          ][
            ask in-myfield-neighbors with [color = grey][
              set color green                                              ; Else, reverse the change
              set current-land-use "pasture"]
          ]
        ]

        if (chosen-strategy = 0 and maizeland > 0)[                      ; Second, check if it is possible to change from MAIZE to FALLOW?????????????????????
          change-from-maize-to-fallow-test                               ; Change a maize field into fallow
          calculate-expected-outcome                                     ; Calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of abandoning land, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

          ifelse income-expectation >= income-threshold-mean and         ; Check if expected outcomes fulfill aspirations
                 food-expectation >= food-threshold-mean and
                 leisure-expectation >= leisure-threshold-mean [
            set chosen-strategy "abandon land"                           ; If they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color white]
          ][
            ask in-myfield-neighbors with [color = grey][
              set color yellow                                           ; Else, reverse the change
              set current-land-use "maize"]
            ]

        ]
      ]

      if (chosen-strategy = 0 and vegetableland > 0)[                    ; Third, check if it is possible to change from VEGETABLE to FALLOW?????????????????????
        change-from-vegetable-to-fallow-test                             ; Change a vegetable field into fallow
        calculate-expected-outcome                                       ; Calculate expected outcomes resulting from the current management + the change

        if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
        if who = test-hh [print (word "expectations of abandoning land, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
        if who = test-hh [print ""]

        ifelse income-expectation >= income-threshold-mean and           ; Check if expected outcomes fulfill aspirations
               food-expectation >= food-threshold-mean and
               leisure-expectation >= leisure-threshold-mean [
          set chosen-strategy "abandon land"                             ; If they do, implement change
          ask in-myfield-neighbors with [color = grey][
            set color white]
        ][
          ask in-myfield-neighbors with [color = grey][
            set color orange                                               ; Else, reverse the change
            set current-land-use "vegetable"]
        ]
      ]

      if (chosen-strategy = 0 and coffeeland > 0 and                     ; Fourth and last, check if it is possible to change from COFFEE to FALLOW?????????????????????
          count in-myfield-neighbors with [coffee-age > 7 and            ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use
                                           current-land-use = "coffee"] > 0) [
        change-from-coffee-to-fallow-test                                ; Change a coffee field into fallow
        calculate-expected-outcome                                       ; Calculate expected outcomes resulting from the current management + the change

        if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
        if who = test-hh [print (word "expectations of abandoning land, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
        if who = test-hh [print ""]

        ifelse income-expectation >= income-threshold-mean and           ; Check if expected outcomes fulfill aspirations
               food-expectation >= food-threshold-mean and
               leisure-expectation >= leisure-threshold-mean [
          set chosen-strategy "abandon land"                             ; If they do, implement change
          ask in-myfield-neighbors with [color = grey][
            set color white]
        ][
          ask in-myfield-neighbors with [color = grey][
            set color brown                                               ; Else, reverse the change
            set current-land-use "coffee" ]
        ]
      ]

    ]

    ; CHANGE TO PASTURE?                                                 ; Check for consecutive land uses whether it is possible to change from that land use to pasture, starting with fallow, maize, vegetables and then coffee.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if (x = "change to pasture" and chosen-strategy = 0) [
      set testing-strategy "change to pasture"
      ; if who = test-hh [print (word "household " who " is testing: " x " | " testing-strategy)]

        if farmland > privatepasture [                                   ; if all of the farmers land has already been changed to pasture, changing to pasture is not possible anymore

          if (chosen-strategy = 0 and fallowland > 0) [                  ; first, check if it is possible to change from FALLOW to PASTURE?????????????????????

            change-from-fallow-to-pasture-test                           ; change a fallow field into pasture
            calculate-expected-outcome                                   ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing to pasture, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            ifelse income-expectation >= income-threshold-mean and       ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [

              set chosen-strategy "change to pasture"                    ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ ask in-myfield-neighbors with [color = grey][
                set color white set current-land-use "fallow"            ; else, reverse change
            ]]
          ]


      if (chosen-strategy = 0 and maizeland > 0) [                       ; second, check if it is possible to change from MAIZE to PASTURE?????????????????????

          change-from-maize-to-pasture-test                            ; change a maize field into pasture
          calculate-expected-outcome                                   ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing from maize to PASTURE, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            ifelse income-expectation >= income-threshold-mean and       ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [

              set chosen-strategy "change to pasture"                    ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ ask in-myfield-neighbors with [color = grey][
                set color yellow set current-land-use "maize"            ; else, reverse change
            ]]
          ]


        if (chosen-strategy = 0 and vegetableland > 0) [                 ; third, check if it is possible to change from VEGETABLES to PASTURE??????????????
            change-from-vegetable-to-pasture-test                        ; change a vegetable field into pasture
            calculate-expected-outcome                                   ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing to pasture, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]


            ifelse income-expectation >= income-threshold-mean and       ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
            set chosen-strategy "change to pasture"                      ; if they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ask in-myfield-neighbors with [color = grey][
              set color orange set current-land-use "vegetable"          ; else, reverse change
          ]]
        ]



        if (chosen-strategy = 0 and coffeeland > 0 and                  ; fourth and last, check if it is possible to change from COFFEE to PASTURE??????????????
            count in-myfield-neighbors with [coffee-age > 7 and         ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use
                                             current-land-use = "coffee"] > 0) [
            change-from-coffee-to-pasture-test                          ; change a coffee field into pasture
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing to pasture, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]


            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
            set chosen-strategy "change to pasture"                     ; if they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ask in-myfield-neighbors with [color = grey][
              set color brown set current-land-use "coffee"]]           ; else, reverse change
          ]
        ]
      ]

      ; CHANGE TO VEGETABLE?                                            ; check for consecutive land uses whether it is possible to change from that land use to vegetable, starting with fallow, maize, pasture, and then coffee.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      if (x = "change to vegetable" and chosen-strategy = 0)[
      set testing-strategy "change to vegetable"
      ; if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

      if farmland > vegetableland [                                     ; if all of the farmers land has already been changed to vegetable, changing to vegetable is not possible anymore

        if (chosen-strategy = 0 and fallowland > 0)[                    ; first, check if it is possible to change from FALLOW to VEGETABLE??????????????????
            change-from-fallow-to-vegetable-test                        ; change a fallow field into vegetable
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color white set current-land-use "fallow"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and maizeland > 0)[                   ; second, check if it is possible to change from MAIZE to VEGETABLE??????????????????
            change-from-maize-to-vegetable-test                         ; change a maize field into vegetable
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color yellow set current-land-use "maize"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and privatepasture > 0)[              ; third, check if it is possible to change from PASTURE to VEGETABLE???????????????????
            change-from-pasture-to-vegetable-test                       ; change a pasture into vegetable
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color green set current-land-use "pasture"]]          ; else, reverse change
          ]

          if (chosen-strategy = 0 and coffeeland  > 0 and               ; fourth, and last, check if it is possible to change from COFFEE to VEGETABLE???????????????????
              count in-myfield-neighbors with [coffee-age > 7 and       ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use
                                               current-land-use = "coffee"] > 0) [
            change-from-coffee-to-vegetable-test                        ; change a coffee field into vegetable
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color brown set current-land-use "coffee"]]           ; else, reverse change
          ]
        ]
      ]

      ; CHANGE TO MAIZE?                                                ; check for consecutive land uses whether it is possible to change from that land use to maize, starting with fallow, vegetable, pasture, and then coffee.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      if (x = "change to maize" and chosen-strategy = 0)[
      set testing-strategy "change to maize"
        ; if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

        if farmland > maizeland [                                       ; if all of the farmers land has already been changed to maize, changing to maize is not possible anymore


          if (chosen-strategy = 0 and fallowland > 0)[                  ; first check if it is possible to change from FALLOW to MAIZE????????????????????
            change-from-fallow-to-maize-test                            ; change a fallow field into maize
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing to maize, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color white set current-land-use "fallow"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and vegetableland > 0)[               ; second, check if it is possible to change from VEGETABLE to MAIZE????????????????????
            change-from-vegetable-to-maize-test                         ; change a vegetable field into maize
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing to maize, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color orange set current-land-use "vegetable"]]       ; else, reverse change
          ]

          if (chosen-strategy = 0 and privatepasture > 0)[              ; third, check if it is possible to change from PASTURE to MAIZE????????????????????????
            change-from-pasture-to-maize-test                           ; change a pasture into maize
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing to maize, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color green set current-land-use "pasture"]]          ; else, reverse change
          ]

          if (chosen-strategy = 0 and coffeeland > 0 and                ; fourth, and last, check if it is possible to change from COFFEE to MAIZE????????????????????????
              count in-myfield-neighbors with [coffee-age > 7 and       ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use
                                               current-land-use = "coffee"] > 0) [
            change-from-coffee-to-maize-test                            ; change a coffee field into maize
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing to maize, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color brown set current-land-use "coffee"]]           ; else, reverse change
          ]
        ]
      ]

      ; PLANT COFFEE?                                                   ; check for consecutive land uses whether it is possible to change from that land use to coffee, starting with fallow, pasture, maize, and then vegetable.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      if (x = "plant coffee" and chosen-strategy = 0)[
       set testing-strategy "plant coffee"
        if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

        if farmland > coffeeland [                                      ; if all of the farmers land has already been changed to maize, changing to maize is not possible anymore

          if (chosen-strategy = 0 and fallowland > 0)[                  ; first, check if it is possible to change from FALLOW to COFFEE????????????????????
            change-from-fallow-to-coffee-test                           ; change a fallow field into coffee
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing from fallow to COFFEE, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            let estimated-cost (costs-of-production +                   ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color white set current-land-use "fallow"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and privatepasture > 0)[              ; second, check if it is possible to change from PASTURE to COFFEE????????????????????
            change-from-pasture-to-coffee-test                          ; change a pasture into coffee
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing from pasture to COFFEE, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            let estimated-cost (costs-of-production +                   ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color green set current-land-use "pasture"]]          ; else, reverse change
          ]

          if (chosen-strategy = 0 and maizeland > 0)[                   ; third, check if it is possible to change from MAIZE to COFFEE????????????????????????
            change-from-maize-to-coffee-test                            ; change a maize field into coffee
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing from maize to COFFEE, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

            let estimated-cost (costs-of-production +                   ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color yellow set current-land-use "maize"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and vegetableland > 0)[               ; fourth, and last, check if it is possible to change from VEGETABLE to COFFEE????????????????????????
            change-from-vegetable-to-coffee-test                        ; change a vegetable field into coffee
            calculate-expected-outcome                                  ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of changing from vegetable to COFFEE, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse income-expectation >= income-threshold-mean and      ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                   food-expectation >= food-threshold-mean and
                   leisure-expectation >= leisure-threshold-mean and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color orange set current-land-use "vegetable"]]       ; else, reverse change
          ]
        ]
      ]

      ; BUY COW?                                                        ; check if buying a cow can satisfice farmers
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      if (x = "buy cow" and chosen-strategy = 0)[
       set testing-strategy "buy cow"
       ; if who = test-hh  [print (word "household " who " is checking whether it can afford: " x " | " testing-strategy)]
      let mycapacity floor (((feedproduction * 1000) / 365 )  / 3.15)

      if (savings > (costs-of-production + global-cow-purchase-price) and ; farmers need to have enough savings to cover the additional cost of buying a cow
          (mycapacity > herd or adopted-feed? = "yes")) [                ; need to be able to feed the new cow
        ; if who = test-hh  [print (word "household " who " can afford: " x " | " testing-strategy " so proceeds to testing")]
         buy-cow-test                                                   ; buy a cow
         calculate-expected-outcome                                     ; calculate expected outcomes resulting from the current management + the change

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of buying cow, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

       ifelse income-expectation >= income-threshold-mean and           ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
              food-expectation >= food-threshold-mean and
              leisure-expectation >= leisure-threshold-mean [
        set chosen-strategy "buy cow"                                   ; if they do, and the farmers have sufficient savings, implement change
       ; if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || accept buy-cow-test, keep change")]
          ask in-mycow-neighbors with [bought? = "yes" and age = 4][
            set color pink + 2]
       ][                                                              ; else, reverse change
          set test-cowcost 0
         ; if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || reverse buy-cow-test")]
          ask in-mycow-neighbors with [bought? = "yes" and age = 4][
            die
          ]
       ]
     ]
   ]

    ; BUY FEED?                                                         ; check if feeding cattle supplementary feed can satisfice. all cattle are given if this choice is made, and once adopted, the choice cannot be reversed.
                                                                        ; future offspring will also be fed supplementary feed.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "buy feed" and chosen-strategy = 0)[
      set testing-strategy "buy feed"
      if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

      let cows-on-supplement count in-mycow-neighbors with [supplementary-feed = "yes"]
      let on-supplement ifelse-value (cows-on-supplement > 0)["yes"]["no"]
      let estimated-cost costs-of-production +
                         (global-supplementary-feed-price * herd)       ; calculate the expected cost of feeding the herd supplementary feed

      if who = test-hh  [print (word "household " who " needs " estimated-cost ", has " savings)]

    if (savings >= estimated-cost and                                   ; need to be able to buy the feed
        herd > 0 and                                                    ; need to have cattle to feed
        on-supplement = "no")[                                          ; and need to not already be feeding supplementary feed
        buy-feed-test                                                   ; adopt supplementary feeding
        calculate-expected-outcome                                      ; calculate expected outcomes resulting from the current management + the change

        if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
        if who = test-hh [print (word "expectations of buying FEED, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
        if who = test-hh [print ""]

        ifelse income-expectation >= income-threshold-mean and          ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
               food-expectation >= food-threshold-mean and
               leisure-expectation >= leisure-threshold-mean [
          set chosen-strategy "buy feed"                                ; if they do, and the farmers have sufficient savings, implement change
          set adopted-feed? "yes"                                       ; this strategy is adopted once, then kept and implemented automatically for new cattle
          ask in-mycow-neighbors with [supplementary-feed = "testing"][
            set supplementary-feed "yes"]
        ][
          set adopted-feed? "no"
          set test-feedcost 0
          ask in-mycow-neighbors with [supplementary-feed = "testing"][
            set supplementary-feed "no"
          ]                                ; else, reverse change
        ]
      ]
    ]

    ; BUY FERTILISER?                                                   ; check if applying more fertiliser can satisfice. households can choose to apply more fertiliser to one field per tick, every field with arable crops is eligible.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "buy fertiliser" and chosen-strategy = 0)[
      set testing-strategy "buy fertiliser"
      ; if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

                                                                        ; number of fields that are eligible for application of fertiliser
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable"] +
                          count in-myfield-neighbors with [current-land-use = "coffee"]
                                                                        ; number of fields have received the maximum amount of fertiliser
      let maxed-out-fields count in-myfield-neighbors with [fertiliser = "very high"]

      if (eligible-fields > 0 and                                       ; there need to be fields that can benefit from fertiliser application
          maxed-out-fields < eligible-fields ) [                        ; and there need to be fields that have not already been maxed out

        buy-fertiliser-test
        let fert-cost calculate-cost-of-fertiliser-test                 ; calculate cost of "testing" field only

        if who = test-hh [ifelse (fert-cost + costs-of-production) <= savings [print (word "has enough savings to buy fertiliser")][print (word "does NOT have enough savings to buy fertiliser")]]
        ifelse (fert-cost + costs-of-production) <= savings [           ; there needs to be enough money to buy fertiliser
          calculate-expected-outcome

          if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
          if who = test-hh [print (word "expectations of buying fertiliser, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
          if who = test-hh [print ""]

          ifelse income-expectation >= income-threshold-mean and        ; check if expected outcomes fulfill aspirations (expectations >= thresholds)
                 food-expectation >= food-threshold-mean and
                 leisure-expectation >= leisure-threshold-mean [
            set chosen-strategy "buy fertiliser"                        ; if they do, and if the hh has sufficient savings, implement change
            ask in-myfield-neighbors [
              set fertiliser remove "testing " fertiliser]              ; take away "testing " from string so that only the fertiliser level remains (low, moderate, high or very high)
          ][
            ask in-myfield-neighbors [                                  ; else, reverse change after checking expectations
              set fertiliser (ifelse-value
                fertiliser = "testing moderate"         ["low"]
                fertiliser = "testing high"             ["moderate"]
                fertiliser = "testing very high"        ["high"]
                                                        [fertiliser]
              )
            ]
          ]
        ][
          ask in-myfield-neighbors [                                   ; else, reverse change from before checking expectations because hh does not have enough money to buy fertiliser
              set fertiliser (ifelse-value
              fertiliser = "testing moderate"             ["low"]
              fertiliser = "testing high"                 ["moderate"]
              fertiliser = "testing very high"            ["high"]
                                                          [fertiliser]
              )
            ]
        ]
      ]
    ]

    ; SELL COW?                                                       ; check if selling cattle can help households fulfill their aspirations
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "sell cow" and chosen-strategy = 0)[
      set testing-strategy "sell cow"
      ; if who = test-hh  [print (word "household " who " is checking whether it can: " x " | " testing-strategy)]
      let mycapacity floor (((feedproduction * 1000) / 365 )  / 3.15)

      if (count in-mycow-neighbors with [calf < 4 and sex = "female"] > 0 and (savings <= 0 or mycapacity < herd))[
        sell-cow-test
        calculate-expected-outcome

        if who = test-hh [print (word "thresholds, income | food | leisure: " round income-threshold-mean " | " precision food-threshold-mean 2 " | " precision leisure-threshold-mean 2)]
        if who = test-hh [print (word "cows that can be sold: " count in-mycow-neighbors with [calf < 4 and sex = "female"])]
        if who = test-hh [print (word "expectations of selling cow, income | food | leisure: " round income-expectation " | " precision food-expectation 2 " | " precision leisure-expectation 2)]
        if who = test-hh [print ""]

        ifelse income-expectation >= income-threshold-mean and
               food-expectation >= food-threshold-mean and
               leisure-expectation >= leisure-threshold-mean [
          set chosen-strategy "sell cow"
          ; if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || accept sell-cow-test, keep change")]
          ask cattle with [color = red][die]
        ][
          ask cattle with [color = red][set color pink + 2]
          set test-cowsales 0
        ]

      ]

    ]



    ; if who = test-hh  [
      ; print (word "end of iteration, x = " x)
      ; print (word "hh " who " chose " chosen-strategy)
      ; print (word "hh " who " expected cowsales: " test-cowsales)
    ; ]
  ] ; end loop

  ; if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || end of loop")]
  ; if who = test-hh  [print ""]

end

to perform-chosen-management                                             ; Sub-functions: calc-maize-prod, calc-maize-profit, calc-vegetable-prod, calc-vegetable-profit, calc-feed-production, age-cattle, ...
                                                                         ;                give-new-calves-supplementary-feed, underfed-die, calc-milk-prod, milk-to-butter, calc-butter-profit, make-oxen, sell-cattle
                                                                         ;                age-coffee, calc-coffee-prod, calc-coffee-profit, calc-labour
  calc-maize-prod
  calc-maize-profit
  calc-vegetable-prod
  calc-vegetable-profit
  calc-feed-production
  age-cattle
  give-new-calves-supplementary-feed
  underfed-die
  calc-milk-prod
  milk-to-butter
  calc-butter-profit
  make-oxen
  sell-cattle
  age-coffee
  calc-coffee-prod
  calc-coffee-profit
  calc-labour
end

to update-hh-characteristics                                             ; No sub-functions
  ask households [
    ; land use
    set farmland sum [field-size] of in-myfield-neighbors
    set maizeland sum [field-size] of in-myfield-neighbors with [current-land-use = "maize"]
    set vegetableland sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable"]
    set privatepasture sum [field-size] of in-myfield-neighbors with [current-land-use = "pasture"]
    set coffeeland sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee"]
    set fallowland sum [field-size] of in-myfield-neighbors with [current-land-use = "fallow"]
    ; cattle
    set herd count in-mycow-neighbors
  ]
end

to calculate-aspirational-outcomes                                       ; No sub-functions
  ask households [
    set value-of-consumed-products maize-consumed * global-maize-price + maizepurchase / 1.2 + (milk-consumed / milk-to-butter-conversion-factor) * global-butter-price + butter-consumed * global-butter-price + coffee-consumed * global-coffee-price + veg-consumed * global-veg-price; birr per household per year
    set food-purchases maizepurchase ; birr per household per year
    set costs-of-production maizecost + vegetablecost + coffeecost + supplementaryfeedcost + cowcost; for all products except butter we haven't added costs yet
    set earnings-from-sales maizesales + vegetablesales + buttersales + bullsales + oxensales + coffeesales + cowsales

    set income-outcome-t1end earnings-from-sales - costs-of-production; can be negative if households have to buy maize (negative maizeprofit), assumes same price for selling and buying, this is not so realistic
    set savings savings + income-outcome-t1end - food-purchases
    set food-outcome-t1end maize-consumed / 56
    set leisure-outcome-t1end ((labourcap - laboursum)) / 56 ; leisure days per week


  ]
end

to calculate-gini                                                        ; No sub-functions
  let g0_10 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.00) ceiling (Number-of-households * 0.10) / sum [income-outcome-t1start] of households
  let g10_25 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.10) ceiling (Number-of-households * 0.25) / sum [income-outcome-t1start] of households
  let g25_50 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.25) ceiling (Number-of-households * 0.50) / sum [income-outcome-t1start] of households
  let g50_75 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.50) ceiling (Number-of-households * 0.75) / sum [income-outcome-t1start] of households
  let g75_90 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.75) ceiling (Number-of-households * 0.90) / sum [income-outcome-t1start] of households
  let g90_100 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.90) ceiling (Number-of-households * 1.00) / sum [income-outcome-t1start] of households

  set gini (0.5 * g0_10 * 10) +  (0.5 * g10_25 * 15 + g0_10 * 15) + (0.5 * g25_50 * 25 + g10_25 * 25) + (0.5 * g50_75 * 25 + g25_50 * 25) + (0.5 * g75_90 * 15 + g50_75 * 15) + (0.5 * g90_100 * 10 + g75_90 * 10)
end

to write-to-file
  if ticks = 0 [
  file-print ( ; column names
    word
    ; basic
      "household;year;Aspiration_adaptation;Strategy_order;time_of_run;optimism;b_no;b_constant;remember_t3;remember_t2;remember_t1"

    ; aspiration outcomes
      ";income_outcome_t1start;income_outcome_t1end;income_outcome_t2;income_outcome_t3"
      ";leisure_outcome_t1start;leisure_outcome_t1end;leisure_outcome_t2;leisure_outcome_t3"
      ";food_outcome_t1start;food_outcome_t1end;food_outcome_t2;food_outcome_t3"
      ";satisficed"

     ; aspiration related outcomes
      ";savings;value_of_consumed-products;food_purchases;costs_of_production;earnings_from_sales"

     ; disaggregated
      ";maizecost;vegetablecost;coffeecost;supplementaryfeedcost;cowcost"
      ";maizesales;vegetablesales;coffeesales;buttersales;oxensales;bullsales;cowsales"

     ; expected costs
      ";test_maizecost;test_vegetablecost;test_coffeecost;test_feedcost;test_cowcost"
      ";test_maizesales;test_vegetablesales;test_coffeesales;test_buttersales;test_oxensales;test_bullsales;test_cowsales"

    ; aspiration thresholds
      ";income_threshold_mean;income_threshold_t1start;income_threshold_t1end;income_threshold_t2;income_threshold_t3"
      ";leisure_threshold_mean;leisure_threshold_t1start;leisure_threshold_t1end;leisure_threshold_t2;leisure_threshold_t3"
      ";food_threshold_mean;food_threshold_t1start;food_threshold_t1end;food_threshold_t2;food_threshold_t3"

    ; aspiration gaps
      ";incomegap"
      ";leisuregap"
      ";foodgap"

    ; aspiration relative importance
      ";relativeimportanceincome"
      ";relativeimportanceleisure"
      ";relativeimportancefood"

    ; experience weight (ew)
      ";ew_income"
      ";ew_leisure"
      ";ew_food"

    ; chosen strategies
      ";chosen_strategy"
      ";income_expectation;leisure_expectation;food_expectation"

    ; household land use
      ";maizeland;vegetableland;coffeeland;privatepasture;fallowland;farmland"
      ";unfertilisedland"
      ";moderatefertilisedland"
      ";highlyfertilisedland"
      ";veryhighlyfertilisedland"
      ";maizefertilisedland"
      ";vegetablefertilisedland"
      ";coffeefertilisedland"

    ; livestock
     ";herd"
     ";on_supplementary_feed"
     ";adopted_feed"


    )
  ]
  ; file-print (word "---------- Tick Number: " ticks "-----------")
  ;; use SORT so the turtles print their data in order by who number,
  ;; rather than in random order
  foreach sort households [ t ->
    ask t [
      file-print (
        word

        ; basic
         self " ;" ticks " ;" Aspiration-adaptation " ;" Strategy-order " ;" date-and-time " ;" optimism " ;" b-no " ;" b-constant " ;" remember-t3 " ;" remember-t2 " ;" remember-t1

        ; aspiration outcomes
        " ;" income-outcome-t1start  " ;" income-outcome-t1end  " ;" income-outcome-t2  " ;" income-outcome-t3
        " ;" leisure-outcome-t1start " ;" leisure-outcome-t1end " ;" leisure-outcome-t2 " ;" leisure-outcome-t3
        " ;" food-outcome-t1start    " ;" food-outcome-t1end    " ;" food-outcome-t2    " ;" food-outcome-t3
        " ;" satisficed

        ; aspiration related outcomes
        " ;" savings " ;" value-of-consumed-products " ;" food-purchases " ;" costs-of-production " ;" earnings-from-sales

        ; disaggregated
        " ;" maizecost " ;" vegetablecost " ;" coffeecost " ;" supplementaryfeedcost " ;" cowcost
        " ;" maizesales " ;" vegetablesales " ;" coffeesales " ;" buttersales " ;" oxensales " ;" bullsales " ;" cowsales

        ; expected costs
        " ;" test-maizecost " ;" test-vegetablecost " ;" test-coffeecost " ;" test-feedcost " ;" test-cowcost
        " ;" test-maizesales " ;" test-vegetablesales " ;" test-coffeesales " ;" test-buttersales " ;" test-oxensales " ;" test-bullsales " ;" test-cowsales

        ; aspiration thresholds
        " ;" income-threshold-mean  " ;" income-threshold-t1start  " ;" income-threshold-t1end  " ;" income-threshold-t2  " ;" income-threshold-t3
        " ;" leisure-threshold-mean " ;" leisure-threshold-t1start " ;" leisure-threshold-t1end " ;" leisure-threshold-t2 " ;" leisure-threshold-t3
        " ;" food-threshold-mean    " ;" food-threshold-t1start    " ;" food-threshold-t1end    " ;" food-threshold-t2    " ;" food-threshold-t3

        ; aspiration gaps
        " ;" income-gap
        " ;" leisure-gap
        " ;" food-gap

        ; aspiration relative importance
        " ;" income-relativeimportance
        " ;" leisure-relativeimportance
        " ;" food-relativeimportance

        ; experience weight (ew)
        " ;" ew-income
        " ;" ew-leisure
        " ;" ew-food

        ; chosen-strategies
        " ;" chosen-strategy
        " ;" income-expectation " ;" leisure-expectation " ;" food-expectation

        ; household land use
        " ;" maizeland " ;" vegetableland " ;" coffeeland " ;" privatepasture " ;" fallowland " ;" farmland
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "low"] ; area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "moderate"] ; area moderately fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "high"] ; area highly fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "very high"] ; area very highly fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "maize"] ; maize area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "vegetable"] ; vegetable area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "coffee"] ; coffee area not fertilised

        ; livestock
        " ;" herd
        " ;" count in-mycow-neighbors with [supplementary-feed = "yes"] ; herd on supplementary feed? yes/no bc either all are or none are
        " ;" adopted-feed?

      )
    ]
  ]
  file-print ""  ;; blank line
end

; Sub functions

; Used in loop-through-strategies
to calculate-expected-outcome                                           ; Sub-functions: calc-maize-prod-test, calc-maize-consume-profit-test, calc-vegetable-prod-test, calc-vegetable-profit-test, calc-feed-prod-test, ...
                                                                        ;                underfed-die-test, calc-milk-prod-test, milk-to-butter-test, calc-butter-profit-test, sell-cattle-test, calc-coffee-prod-test,
                                                                        ;                calc-coffee-profit-test, calc-labour-test
  calc-maize-prod-test
  calc-maize-consume-profit-test
  set food-expectation test-maizeconsume / 56
  calc-vegetable-prod-test
  calc-vegetable-profit-test
  calc-feed-prod-test

  underfed-die-test ; buy cow here if that is the chosen strategy
  calc-milk-prod-test
  milk-to-butter-test
  calc-butter-profit-test
  sell-cattle-test

  calc-coffee-prod-test
  calc-coffee-profit-test
  calc-labour-test
  let earnings-from-sales-test test-maizesales + test-vegetablesales + test-buttersales + test-bullsales + test-oxensales + test-cowsales + buy-cow-expected-sales-test + test-coffeesales
  let costs-of-production-test test-maizecost + test-vegetablecost + test-cowcost + test-feedcost + test-coffeecost
  set income-expectation earnings-from-sales-test - costs-of-production-test; can be negative if households have to buy maize (negative maizeprofit), assumes same price for selling and buying, this is not so realistic

end

to change-from-pasture-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-maize-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "maize"][
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-vegetable-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-coffee-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "coffee" and coffee-age > 7][
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-fallow-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-maize-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "maize"][
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-vegetable-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-coffee-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "coffee" and coffee-age > 7][
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-fallow-to-vegetable-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "vegetable"
    ]
end

to change-from-maize-to-vegetable-test
    ask one-of in-myfield-neighbors with [current-land-use = "maize"][
      set color grey
      set current-land-use "vegetable"
    ]
end

to change-from-pasture-to-vegetable-test
  ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
    set color grey
    set current-land-use "vegetable"
  ]
end

to change-from-coffee-to-vegetable-test
    ask one-of in-myfield-neighbors with [current-land-use = "coffee"][
      set color grey
      set current-land-use "vegetable"
    ]
end

to change-from-fallow-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-vegetable-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-pasture-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-coffee-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "coffee"][
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-fallow-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "coffee"
    ]
end

to change-from-pasture-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
      set color grey
      set current-land-use "coffee"
    ]
end

to change-from-maize-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "maize"][
      set color grey
      set current-land-use "coffee"
    ]
end

to change-from-vegetable-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
      set color grey
      set current-land-use "coffee"
    ]
end

to buy-cow-test
  hatch-cattle 1 [
    set color orange
    set size 6
    set bought? "yes"
    set age 4
    set sex "female"
    set dry ifelse-value (random-float 1 < 0.5) ["yes"]["no"]
    set pregnant dry
    set in-lactation ifelse-value (pregnant = "yes")["no"]["yes"]
    set supplementary-feed ifelse-value (any? in-mycow-neighbors with [supplementary-feed = "yes"])["yes"]["no"]
  ]

  ;ask cattle-here[create-mycows-with other households-here]
  create-mycows-with other cattle-here with [bought? = "yes" and age = 4][set color orange]
  set test-herd count in-mycow-neighbors
end

to buy-feed-test
  ask in-mycow-neighbors with [supplementary-feed != "yes"] [
    set supplementary-feed "testing"
  ]
end

to buy-fertiliser-test
  ;print (word "entering buy-fertiliser-test for household: " who)
  if any? (in-myfield-neighbors with [(current-land-use != "fallow" and current-land-use != "pasture") and (fertiliser = "low" or fertiliser = "moderate" or fertiliser = "high")])
    [
      ask one-of (in-myfield-neighbors with [
        (current-land-use != "fallow" and current-land-use != "pasture") and (fertiliser = "low" or fertiliser = "moderate" or fertiliser = "high")])[

        set fertiliser (ifelse-value
          fertiliser = "low" ["testing moderate"]
          fertiliser = "moderate" ["testing high"]
          fertiliser = "high" ["testing very high"]
        )

        ;print (word fertiliser " fertiliser for " current-land-use " field " [who] of self " of household " [who] of myself)
      ]
    ]
end

to sell-cow-test
  ask one-of in-mycow-neighbors with [calf < 4 and sex = "female"][
    set color red]
  set test-herd herd - 1
end


; Used in perform-chosen-management

to calc-maize-prod ;

  ask fields with [current-land-use = "maize"][
    (ifelse
      fertiliser = "low" [set yield  maize-yield-low ] ; yield units = kg per ha  source yield: van Dijk 2020 ; parameter maize-yield-low = 2.718
      fertiliser = "moderate" [set yield  maize-yield-moderate] ; for yields > 0, we calculated based on mpp formula estimated based on Assefa 2021 ; parameter maize-yield-medium = 3.331
      fertiliser = "high" [set yield  maize-yield-high] ; parameter maize-yield-high = 4.356
      fertiliser = "very high" [set yield  maize-yield-veryhigh] ; parameter maize-yield-veryhigh = 4.664
  )
    set production yield * field-size ; production unit = kg

  ]

  ask households with [maizeland > 0][ ;ask household 12 [ask in-myfield-neighbors [show production]]
    set maizeproduction sum [production] of in-myfield-neighbors with [current-land-use = "maize"]; unit = kg
  ]

end

to calc-maize-profit

  ; first calculate maize need
  ; assume 60% of calories should come from maize
  ; assume men need 3000 calories, women 2000 calories and children 1500 calories
  ; if aspiration is to consume more than that, however (food-threshold-t1start), then we use that instead of requirement
  ; for now we have no age or gender distribution, so assume on average calorie need is 2000 per household member, meaning each household member needs to consume 1200 calories from maize. This is >2x as much maize as they normally eat, but maize also represents other cereals and enset, which together are the main part of diet (Berhane 2011b, p.1)

  ask households[
    let adult-requirement adults * (2200 + 2900) / 2 * 365
    let elderly-requirement elders * (2300 + 1900) / 2 * 365
    let child-requirement children * (1300 + 1800 + 2000) / 3 * 365
    let hh-requirement adult-requirement + elderly-requirement + child-requirement
    set food-requirement hh-requirement * 0.86 * 0.8 ; see calc-test-maize-consume-profit-test formula for explanation of assumptions
    let maize-requirement food-requirement / 3640 ; unit = kg per hh/year, https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/
    ifelse maize-requirement > (food-threshold-t1start * 56) [
      set maize-left maizeproduction - maize-requirement
      set maize-consumed maize-requirement
    ][
      set maize-left maizeproduction - (food-threshold-t1start * 56)
      set maize-consumed (food-threshold-t1start * 56) ; per yea
    ] ;food-threshold-t1start is the aspiration threshold for food self-sufficiency measured as kg maize consumed per hh per week
    ifelse maize-left >= 0 [
      set maizeprofit maize-left * global-maize-price ; parameter maize-price = 10 ; average domestic maize price per kg over the last years (2009-2018) was 4.8 birr (Minten 2020b, p. 290), but the since the conflict and the drought it has been reported being >20 birr at wholesale (http://www.egte-ethiopia.com/en/in-t-market/commodity-statistics.html)
      set maizepurchase 0
    ][
      set maizeprofit 0
      set maizepurchase (maize-consumed - maizeproduction) * global-maize-price * 1.2 ; parameter for now assuming that market price is 20% higher than farm-gate price
    ]

    set maizesales maizeprofit

    ; subtract cost of fertiliser
    set maizecost calculate-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "maize"]

    set maizeprofit maizeprofit - maizecost

  ]

end

to calc-vegetable-prod
  ask fields with [current-land-use = "vegetable"][
    ; set yield 107 * 100; kg per ha, see CSA 2012, THE FEDERAL DEMOCRATIC REPUBLIC OF ETHIOPIA  CENTRAL STATISTICAL AGENCY AGRICULTURAL SAMPLE SURVEY  2011 / 2012 (2004 E.C.) <- onion
    (ifelse
      fertiliser = "low" [set yield vegetable-yield-low] ; parameter veg-yield-low = 14.9
      fertiliser = "moderate" [set yield vegetable-yield-moderate] ; parameter veg-yield-medium = 38.2
      fertiliser = "high" [set yield vegetable-yield-high] ; parameter veg-yield-high = 41.9
      fertiliser = "very high" [set yield vegetable-yield-veryhigh] ; parameter veg-yield-veryhigh = 56.0
    )
    set production yield * field-size
  ] ; kg per ha

  ask households with [vegetableland > 0][
    set vegetableproduction sum [production] of in-myfield-neighbors with [current-land-use = "vegetable"] ; unit = kg
  ]

end

to calc-vegetable-profit
  ask households[
    let veg-need (household-size / 5) * 5 * 12; unit = kg/year, https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/
    let veg-left vegetableproduction - veg-need
    ifelse veg-left > 0 [
      set vegetableprofit veg-left * global-veg-price
      set veg-consumed veg-need
    ][
      set vegetableprofit 0
      set veg-consumed ifelse-value vegetableproduction > 0 [vegetableproduction][0]
    ] ;

    set vegetablesales vegetableprofit

    ; subtract costs of fertiliser
    set vegetablecost calculate-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "vegetable"]
    set vegetableprofit vegetableprofit - vegetablecost
  ]
end

to calc-feed-production
  ask households [set feedproduction ; in tonnes DM per year
    privatepasture * utilisable-pasture + ; 2 tonnes per ha, of which 75% is utilisable
    maizeland * (utilisable-greenmaizestover + utilisable-drymaizestover) * 0.69 + ; 0.91 tonnes green stover and 1.5 tonnes dry stover (Fernandez-Rivera 2007), which respectively have 91 (ILRI feeds database) and 93% (feedipedia) DM. 69% is utilised (Fernandex-Rivera 2007)
    coffeeproduction * (utilisable-coffeehusk + utilisable-coffeeleaves)]
end

to age-cattle

  ; grow
  ask cattle [
    set age age + 1
    set size (age / 8) + 1
  ]

  ; die
  ask cattle with [age = 14][die]

  ; pregnant cattle -> birth + milk
  ask cattle with [pregnant = "yes"][
    set pregnant "no more"
    if calf < 4 [set calf calf + 1]
    set dry "no"
    set in-lactation "soon"
    set pcolor white
  ]

  ; sprount calves
  ask patches with [pcolor = white][sprout-cattle 1 set pcolor black]

  ; assign sex to calves
  ask n-of (count cattle with [shape = "default"] / 2) cattle with [shape = "default"] [set sex "female" set color pink + 2]
  ask cattle with [sex != "female" and shape = "default"] [set sex "male" set color sky + 2]

  ask cattle with [shape = "default"][
    set shape "cow"
    set age 0
    set dry "yes"
    set calf 0
    set lactation 0
    set size age / 8 + 1
    set bought? "no"
    create-mycalves-with other cattle-here with [pregnant = "no more"]; links calves to their mothers
    forward 2
  ]

  ; link new calves to households
  set cowiter -1
  set all-cattle sort([who] of cattle)
  while[cowiter < length [who] of cattle - 1]
  [
   set cowiter cowiter + 1
   set cowid item cowiter all-cattle ; problem below is cause by friendships and influencer links. This means there are more households 2 distance from cows, and that households will own the cows of their friends.
    (foreach sort([who] of households) n-values length sort([who] of households) [cowid] [ [a b] ->
      ask household a [if nw:distance-to cow b = 2 [ ; first check if the network distance between household and calf = 2
      let pathcheck word item 1 nw:turtles-on-path-to cow b " is a hh? "     ; then check if path is household -> cow -> calf and not household -> household -> cow
      if not member? "household" pathcheck [
          create-mycow-with cow b]

    ] ] ])
  ]

  ; get cattle that have been in-lactation to become pregnant again
  ask cattle with [in-lactation = "yes"][
   set in-lactation "no more"
   if lactation < 4 [set lactation lactation + 1]
   set dry "yes"
  ]

  ; cattle that get pregnant for the first time
  ask n-of (count cattle with [sex = "female" and age = 3] / 2) cattle with [sex = "female" and age = 3][set pregnant "yes"] ; half get pregnant at three years old
  ask cattle with [sex = "female" and age = 4 and in-lactation != "yes"][set pregnant "yes"] ; others get pregnant at four years old

  ; get cattle that have been pregnant before (and were lactating previous year) pregnant
  ask cattle with [sex = "female" and in-lactation = "no more" and calf < 4][set pregnant "yes"]

  ; set cattle that gave birth in-lactation
  ask cattle with [in-lactation = "soon"][set in-lactation "yes"]

end

to give-new-calves-supplementary-feed
  ask households with [adopted-feed? = "yes"][
      ask in-mycow-neighbors [set supplementary-feed "yes"]
  ]
end

to underfed-die
  ask households [
    ;let cows-on-supplement count in-mycow-neighbors with [supplementary-feed = "yes"]
    ;let on-supplement ifelse-value (cows-on-supplement > 0)["yes"]["no"]
    set herd count in-mycow-neighbors

    ; on low-quality diet
    if (herd > 0 and adopted-feed? != "yes" )[
      let capacity floor(((feedproduction * 1000) / 365 ) / 3.15) ; feedproduction * 1000 --> feed production in kg ; feed production / 365 --> feed production per day
      let doomed herd - capacity
      if doomed > 0 [ask n-of doomed in-mycow-neighbors [
        die]
    ]
  ]
    ; on high-quality diet no cows will die due to supplementary feed
]
end

to calc-milk-prod
  ask households with [herd > 0] [
    ask in-mycow-neighbors [
      let tonnes-year ([feedproduction] of myself / [herd] of myself)
      let kg-day (tonnes-year * 1000) / 365
      set feed-quantity kg-day
    ]
  ]

  ; milk yield of cattle on low-quality diet
  ask cattle with [in-lactation = "yes" and supplementary-feed != "yes"]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 3.15 [set milkyield 0 ] ;set color red
      feed-quantity > 3.15 and feed-quantity < 20 [set milkyield -2.11 + 0.67 * feed-quantity ] ;set color green
      feed-quantity >= 20 [set milkyield -2.11 + 0.67 * 20 ] ; max ; set color grey
    )
  ]

  ; milk yield of cattle on high-quality diet
  ask cattle with [in-lactation = "yes" and supplementary-feed = "yes"]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 0 [set milkyield 0 ] ;set color red
      feed-quantity > 0 and feed-quantity < 20 [set milkyield 1.38 + 0.74 * feed-quantity ] ;set color green
      feed-quantity >= 20 [set milkyield 1.38 + 0.74 * 20 ] ; max ; set color grey
    )
  ]

  ask households with [herd > 0]
  [
    set milkproduction sum [milkyield] of in-mycow-neighbors
  ]

end

to milk-to-butter
  ask households [
    let milk-requirement 0.1 * 365 * household-size; unit = L/household member per year
    let milk-left milkproduction - milk-requirement
    ifelse milk-left > 0 [set milk-consumed milk-requirement][set milk-consumed ifelse-value milkproduction > 0 [milkproduction][0]]
    ifelse milk-left > 56 * 5 [set butterproduction milk-left / milk-to-butter-conversion-factor] [set butterproduction 0] ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week.
  ]
end

to calc-butter-profit

  ask households with [milkproduction > 0]
  [
    let butter-requirement 0.2 * 12 * household-size
    let butter-left butterproduction - butter-requirement
    ifelse butter-left > 0 [
      set butterprofit butter-left * global-butter-price
      set butter-consumed butter-requirement
    ][
      set butterprofit 0
      set butter-consumed ifelse-value butterproduction > 0 [butterproduction][0]
    ]

    set buttersales butterprofit
  ]

  ask households [

    ; subtract costs of buying cattle
    set cowcost ifelse-value (chosen-strategy = "buy cow") [global-cow-purchase-price][0]

    ; subtract costs of supplementary feed
    let cows-on-supplementary-feed count in-mycow-neighbors with [supplementary-feed = "yes"]
    set supplementaryfeedcost ifelse-value adopted-feed? = "yes" [cows-on-supplementary-feed * global-supplementary-feed-price] [0]

    ; calculate profit from butter sales subtracting the cost of feed and new cattle
    set butterprofit ifelse-value milkproduction > 0 [buttersales - supplementaryfeedcost - cowcost][0 - supplementaryfeedcost - cowcost]
    ;set butterprofit buttersales - supplementaryfeedcost - cowcost
  ]

end

to make-oxen
  ; assume oxen can start ploughing at age two
  ask households with [herd > 0 and count in-mycow-neighbors with [sex = "male"] > 0][
    let males count in-mycow-neighbors with [sex = "male"]
    if count in-mycow-neighbors with [castrated = "yes"] < 2 [ ; assume
      ask n-of 1 in-mycow-neighbors with [sex = "male"][
        set castrated "yes"
      ]
    ]
    ask in-mycow-neighbors with [castrated = "yes" and age > 1][set color 87]
  ]

  ask households[set oxen count in-mycow-neighbors with [castrated = "yes" and age > 1]]

end

to sell-cattle
  ; consume old dairy cattle
  ask households with [count in-mycow-neighbors with [calf = 4] > 0][
    ask in-mycow-neighbors with [calf = 4][die]
  ]

  ; sell 2 year old bulls
  ask households with [count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] = 0][
    set bullsales 0
  ]

  ask households with [count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] > 0][
    let to-sell count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1]
    set bullsales to-sell * global-bull-sell-price ;etb
    ask in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1][die]
  ]

  ; sell 7 year old oxen
  ask households with [count in-mycow-neighbors with [castrated = "yes" and age > 6] = 0] [
    set oxensales 0
  ]

  ask households with [count in-mycow-neighbors with [castrated = "yes" and age > 6] > 0][
    let to-sell count in-mycow-neighbors with [castrated = "yes" and age > 6]
    set oxensales to-sell * global-oxen-sell-price ; etb
    ask in-mycow-neighbors with [castrated = "yes" and age > 6][die]
  ]

  ; sell cows if this option is chosen
  ; ask households [set cowsales test-cowsales]

end

to age-coffee
  ask fields with [current-land-use = "coffee"][
    set coffee-age coffee-age + 1
    if coffee-age > 30 [set current-land-use "fallow" set color white]
  ]
  ask households with [count in-myfield-neighbors with [current-land-use = "coffee"] * area-field <= 0][set coffeeproduction 0 set coffeeprofit 0]
end

to calc-coffee-prod
  ask fields with [current-land-use = "coffee"][

    if coffee-age <= 3 [ ;set yield 50.34
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-establishment]
        fertiliser = "moderate" [set yield coffee-yield-moderate-establishment]
        fertiliser = "high" [set yield coffee-yield-high-establishment]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-establishment]
      )
    ] ;kg per ha
    if coffee-age >= 3 and coffee-age <= 6 [
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-initiation]
        fertiliser = "moderate" [set yield coffee-yield-moderate-initiation]
        fertiliser = "high" [set yield coffee-yield-high-initiation]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-initiation]
      )
    ] ;kg per ha
    if coffee-age >= 7 and coffee-age <= 27 [
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-full]
        fertiliser = "moderate" [set yield coffee-yield-moderate-full]
        fertiliser = "high" [set yield coffee-yield-high-full]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-full]
      )
    ] ;kg per ha
    if coffee-age >= 28 and coffee-age <= 30 [; set yield 453.02
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-ageing]
        fertiliser = "moderate" [set yield coffee-yield-moderate-ageing]
        fertiliser = "high" [set yield coffee-yield-high-ageing]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-ageing]
      )
    ] ;kg per ha

    set production yield * field-size
  ]

  ask households with [coffeeland > 0][
    set coffeeproduction sum [production] of in-myfield-neighbors with [current-land-use = "coffee"] ; unit = kg
  ]
end

to calc-coffee-profit
  ask households with [coffeeproduction > 0][
    let coffee-requirement 0.2 * 12 * household-size ; according to https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/ ; parameter
    let coffee-left coffeeproduction - coffee-requirement
    ifelse coffee-left > 0 [
      set coffeeprofit coffee-left * global-coffee-price
      set coffee-consumed coffee-requirement
    ][
      set coffeeprofit 0
      set coffee-consumed ifelse-value coffeeproduction > 0 [coffeeproduction][0]
    ] ;according to Diro 2019 ; parameter

    set coffeesales coffeeprofit

    ; subtract planting and maintanance costs
    ;let phaseI count in-myfield-neighbors with [coffee-age = 1] * global-coffeeplantation-price
    let phaseI (sum [field-size] of in-myfield-neighbors with [coffee-age = 1]) * global-coffeeplantation-price ; etb per ha --> reference = Diro 2019 ; parameter
    let phaseII (sum [field-size] of in-myfield-neighbors with [coffee-age = 2  and coffee-age = 3]) * 19053 ; etb per ha ; parameter
    let phaseIII (sum [field-size] of in-myfield-neighbors with [coffee-age >= 4  and coffee-age <= 8]) * 22039 ; etb per ha ; parameter
    let phaseIV (sum [field-size] of in-myfield-neighbors with [coffee-age >= 9  and coffee-age <= 12]) * 18247 ; etb per ha ; parameter
    let phaseV (sum [field-size] of in-myfield-neighbors with [coffee-age >= 13]) * 19843 ; etb per ha ; parameter
    let coffeecost-maintain phaseI + phaseII + phaseIII + phaseIV + phaseV

    ; subtract fertiliser costs
    let coffeecost-fertiliser calculate-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "coffee"]
    set coffeecost coffeecost-maintain + coffeecost-fertiliser
    set coffeeprofit coffeeprofit - coffeecost
  ]

  ask households with [coffeeland <= 0][set coffeecost 0 set coffeesales 0]
end

to calc-labour
  ask households [
    let maizelabour maizeland * 92.8 ; person days per year ; parameter maize-labour-needed = 98.2
    let coffeelabour coffeeland * 132 ; person days per year ; parameter coffee-labour-needed = 132
    let coffeeinitialisationlabour count in-myfield-neighbors with [current-land-use = "coffee" and coffee-age = 0] * area-field * 51 ; person days to establish plantation first year ; coffee-initialisation-labour-needed = 51
    let vegetablelabour vegetableland * 92.8 + (92.8 * 0.15) ; person days per yeaer ; vegetable-labour-needed = 106.72
    let cattlelabour (2.55 - 0.037 * herd) * herd ; parameter cattle-labour-needed-intercept = 2.55 parameter cattle-labour-needed-slope = - 0.037
    let pasturelabour 0 ; find out later
    set laboursum maizelabour + coffeelabour + coffeeinitialisationlabour + vegetablelabour + cattlelabour + pasturelabour    ; labour days per year
  ]
end

; Used in calculate-expected-outcome

to calc-maize-prod-test ;
  ask in-myfield-neighbors with [current-land-use = "maize"][
    ;set test-yield 3.4 * 1000; kg per ha, see Assefa et. al. 2021, Usage and Impacts of Technologies and Management Practices in Ethiopian Smallholder Maize Production <- later, allow for spatial differences based on agro-climate and differences based on inputs
    (ifelse
      fertiliser = "low" [set test-yield  maize-yield-low ] ; yield units = kg per ha  source yield: van Dijk 2020
      fertiliser = "moderate" or fertiliser = "testing moderate" [set test-yield  maize-yield-moderate] ; for yields > 0, we calculated based on mpp formula estimated based on Assefa 2021
      fertiliser = "high" or fertiliser = "testing high" [set test-yield  maize-yield-high]
      fertiliser = "very high" or fertiliser = "testing very high" [set test-yield  maize-yield-veryhigh]
  )
    set test-production test-yield * field-size ; unit = kg
  ]

  set test-maizeproduction sum [test-production] of in-myfield-neighbors with [current-land-use = "maize"]; unit = kg
  ;if who = test-hh [print (word "test-maizeproduction = " test-maizeproduction)]

end

to calc-maize-consume-profit-test
  let adult-requirement adults * (2200 + 2900) / 2 * 365 ; unit = calories per year (Claro 2010 https://www.scielo.br/j/csp/a/KVZN6Bp7Wx633qkXKTzG4Gh/?format=pdf&lang=en)
  let elderly-requirement elders * (2300 + 1900) / 2 * 365
  let child-requirement children * (1300 + 1800 + 2000) / 3 * 365
  let hh-requirement adult-requirement + elderly-requirement + child-requirement
  set food-requirement hh-requirement * 0.86 * 0.8; 0.86 = typical proportion of caloric intake coming from staple grains (https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/); 0.8 = amount households are willing to consumption smoothe; households commonly lower their intake in order to survive without selling of assets
  let maize-requirement food-requirement / 3640 ; conversion to kg hh/year (https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/)

  ifelse maize-requirement > (food-threshold-t1start * 56) [
    set maize-left test-maizeproduction - maize-requirement
    set test-maizeconsume maize-requirement
  ][
    set maize-left test-maizeproduction - (food-threshold-t1start * 56)
    set test-maizeconsume (food-threshold-t1start * 56)
  ] ;food-threshold-t1start is the aspiration threshold for food self-sufficiency measured as kg maize consumed per hh per week

  ifelse maize-left >= 0 [
    set test-maizesales maize-left * global-maize-price ; parameter maize-price = 10 ; average domestic maize price per kg over the last years (2009-2018) was 4.8 birr (Minten 2020b, p. 290), but the since the conflict and the drought it has been reported being >20 birr at wholesale (http://www.egte-ethiopia.com/en/in-t-market/commodity-statistics.html)
    let test-maizepurchase 0
  ][
    set test-maizesales 0
    let test-maizepurchase maize-left * global-maize-price * 1.2 ; parameter for now assuming that market price is 20% higher than farm-gate price
  ]

  ; if who = test-hh [print (word "test-maizesales = " test-maizesales)]

  ; subtract cost of fertiliser
  set test-maizecost calculate-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "maize"]
  if any? in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "testing"] [set test-maizecost test-maizecost + calculate-cost-of-fertiliser-test]

end

to calc-vegetable-prod-test ;
  ask in-myfield-neighbors with [current-land-use = "vegetable"][
    (ifelse
      fertiliser = "low" [set test-yield vegetable-yield-low]
      fertiliser = "moderate" or fertiliser = "testing moderate" [set test-yield vegetable-yield-moderate]
      fertiliser = "high" or fertiliser = "testing high" [set test-yield vegetable-yield-high]
      fertiliser = "very high" or fertiliser = "testing very high" [set test-yield vegetable-yield-veryhigh]
    )
    set test-production test-yield * field-size ; unit = kg
  ]

  set test-vegetableproduction sum [test-production] of in-myfield-neighbors with [current-land-use = "vegetable"]; unit = kg

end

to calc-vegetable-profit-test
  let veg-need (household-size / 5) * 5 * 12; unit = kg/year, https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/
  let veg-left test-vegetableproduction - veg-need
  ifelse veg-left > 0 [set test-vegetablesales veg-left * global-veg-price][set test-vegetablesales 0]

  ; subtract cost of fertiliser
  set test-vegetablecost calculate-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "vegetable"] ; fields with fertiliser
  ; if who = test-hh [print (word "who = " who " this is inside the calc-vegetable-profit-test, fertiliser cost of adopted fields = " test-vegetablecost)]
  ; if who = test-hh [print (word "who = " who " this is inside the calc-vegetable-profit-test, fertiliser cost of test fields = " calculate-cost-of-fertiliser-test)]
  set test-vegetablecost test-vegetablecost + calculate-cost-of-fertiliser-test  ; fields testing fertiliser
  ; if who = test-hh [print (word "who = " who " this is inside the calc-vegetable-profit-test, test-vegetablecost = " test-vegetablecost)]

end

to calc-coffee-prod-test
  ask fields with [current-land-use = "coffee"][ ; here I choose the yield associated with phase III, which is the yield at full production
    ; set yield 906.04 ; yield at max production
    (ifelse
        fertiliser = "low" [set yield 906.04]
        fertiliser = "moderate" [set yield 1694]
        fertiliser = "high" [set yield 2016]
        fertiliser = "very high" [set yield 1864.44]
      )
    set production yield * field-size
  ]

  set test-coffeeproduction sum [production] of in-myfield-neighbors with [current-land-use = "coffee"] ; unit = kg
end

to calc-feed-prod-test
  set test-feedproduction sum [field-size] of in-myfield-neighbors with [current-land-use = "pasture"] * 0.75 * 2 +
                          sum [field-size] of in-myfield-neighbors with [current-land-use = "maize"] * 0.3 * 3.29 * 0.99 +
                          test-coffeeproduction * 0.3 * 3 +
                          test-coffeeproduction * 1 * 0.18 ; in tonnes DM per year
end

to underfed-die-test

    set test-cowcost ifelse-value testing-strategy = "buy cow" [global-cow-purchase-price / 10] [0] ; divided by ten because the costs are payed off over the entire lifetime of the cow, not the same year only
    set test-feedcost ifelse-value (testing-strategy = "buy feed" or adopted-feed? = "yes") [(global-supplementary-feed-price * count in-mycow-neighbors)] [0]

    let sell-heifer?    ifelse-value any? in-mycow-neighbors with [color = red and calf < 1][1][0]
    let sell-young-cow? ifelse-value any? in-mycow-neighbors with [color = red and calf = 1][1][0]
    let sell-cow?       ifelse-value any? in-mycow-neighbors with [color = red and calf > 1][1][0]

    set test-cowsales sell-heifer? * global-heifer-sell-price +           ; female cattle that have not yet given birth yet are heifers
                      sell-young-cow? * global-cow-sell-price * 2 +       ; price of young milk cows = 2x price of older milk cows, source: Vernooij 2010
                      sell-cow? * global-cow-sell-price                   ; price of milk cows

     ; if who = test-hh [ print (word "who = " who " this is inside the underfed-die-test, test-cowsales = " test-cowsales)]

    ; only cows on low quality diet risk dying
    if (count in-mycow-neighbors with [color != red] > 0 and testing-strategy != "buy feed" and adopted-feed? != "yes") [
      let doomed ceiling(((test-feedproduction * 1000) / 365 ) / 3.15)
      if doomed > count in-mycow-neighbors with [color != red] [set doomed count in-mycow-neighbors with [color != red]] ; can happen due to rounding up
      ask n-of doomed in-mycow-neighbors with [color != red] [set color yellow] ; I don't want them to die when I am testing, so turn them yellow instead
    ]

end

to calc-milk-prod-test
    ask in-mycow-neighbors with [color != yellow and color != red][ ; yellow cows are doomed to die of under-nutrition, red cows are sold, orange cows are bought
      ifelse ([count in-mycow-neighbors] of myself = 0)[
        set feed-quantity 0][
        let tonnes-year ([feedproduction] of myself / [count in-mycow-neighbors] of myself)
        let kg-day (tonnes-year * 1000) / 365
        set feed-quantity kg-day]
    ]

  ; milk yield with low-quality diet
  ask cattle with [in-lactation = "yes" and color != yellow and color != red and color != orange and supplementary-feed = "no"]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 3.15 [set milkyield 0 ] ;set color red
      feed-quantity > 3.15 and feed-quantity < 20 [set milkyield -2.11 + 0.67 * feed-quantity ] ;set color green
      feed-quantity >= 20 [set milkyield -2.11 + 0.67 * 20 ] ; this is max;  set color grey
    )
  ]

  ; milk yield with high-quality diet
  ask cattle with [in-lactation = "yes" and color != yellow and color != red and color != orange and (supplementary-feed = "testing" or supplementary-feed = "yes")]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 0 [set milkyield 0 ] ;set color red
      feed-quantity > 0 and feed-quantity < 20 [set milkyield 1.38 + 0.74 * feed-quantity ] ;set color green
      feed-quantity >= 20 [set milkyield 1.38 + 0.74 * 20 ] ; this is max;  set color grey
    )
  ]

  if count in-mycow-neighbors with [color != yellow and color != red] > 0 [ set test-milkproduction sum [milkyield] of in-mycow-neighbors ]
  if testing-strategy = "stick with current" and who = test-hh [print (word "milk production current = " test-milkproduction)]
  if testing-strategy = "buy feed" and who = test-hh [print (word "milk production supplementary feed = " test-milkproduction)]

end

to milk-to-butter-test
   let milk-requirement 0.1 * 365 * household-size; unit = L/household member per year
   let milk-left test-milkproduction - milk-requirement
   ifelse milk-left > 56 * 5 [set test-butterproduction milk-left / milk-to-butter-conversion-factor] [set test-butterproduction 0] ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week.
  ; About 16.5 litres of milk is required to produce a kilogram of butter (Anteneh 2010, p. 23)
end

to calc-butter-profit-test
  let butter-requirement 0.2 * 12 * household-size
  let butter-left test-butterproduction - butter-requirement
  ifelse butter-left > 0 [set test-buttersales (butter-left * global-butter-price)][set test-buttersales 0]
end

to sell-cattle-test

  ; sell 2 year old bulls
  if count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] = 0 [ set test-bullsales 0 ]
  if count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] > 0[
    let to-sell count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1]
    set test-bullsales to-sell * global-bull-sell-price ;etb
  ]

  ; sell 7 year old oxen
  if count in-mycow-neighbors with [castrated = "yes" and age > 6] = 0 [ set test-oxensales 0 ]
  if count in-mycow-neighbors with [castrated = "yes" and age > 6] > 0[
    let to-sell count in-mycow-neighbors with [castrated = "yes" and age > 6]
    set test-oxensales to-sell * global-oxen-sell-price ; etb
    ]
end

to-report buy-cow-expected-sales-test
  ifelse testing-strategy = "buy cow" [
    ifelse count in-mycow-neighbors with [color != orange and in-lactation = "yes"] > 0 [
      let average-buttersales-percow ( test-buttersales / count in-mycow-neighbors with [color != orange and sex = "female"] ) * 4 ; this is the amount of milk she is expected to produce over her life
      let exp-bullsales ( round sum (list random-float 1 random-float 1 random-float 1 random-float 1) ) * global-bull-sell-price  ; this is the number of bulls she is expected to sell (0-4)
      report (average-buttersales-percow + exp-bullsales) / 10                                                                     ; this is the average income from sales over her life
    ][
      ask in-mycow-neighbors with [in-lactation = "yes" and color = orange and supplementary-feed = "no"][
        (ifelse
          feed-quantity <= 3.15 [set milkyield 0 ] ;set color red
          feed-quantity > 3.15 and feed-quantity < 20 [set milkyield -2.11 + 0.67 * feed-quantity ] ;set color green
          feed-quantity >= 20 [set milkyield -2.11 + 0.67 * 20 ] ; this is max;  set color grey
        )
      ]

      ; milk yield with high-quality diet
      ask cattle with [in-lactation = "yes" and color = orange and (supplementary-feed = "testing" or supplementary-feed = "yes")]; with [some age criteria]
      [
        (ifelse
          feed-quantity <= 0 [set milkyield 0 ] ;set color red
          feed-quantity > 0 and feed-quantity < 20 [set milkyield 1.38 + 0.74 * feed-quantity ] ;set color green
          feed-quantity >= 20 [set milkyield 1.38 + 0.74 * 20 ] ; this is max;  set color grey
        )
      ]

      set test-milkproduction sum [milkyield] of in-mycow-neighbors with [color = orange]

      let milk-requirement 0.1 * 365 * household-size; unit = L/household member per year
      let milk-left test-milkproduction - milk-requirement
      ifelse milk-left > 56 * 5 [set test-butterproduction milk-left / milk-to-butter-conversion-factor] [set test-butterproduction 0] ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week.

      let butter-requirement 0.2 * 12 * household-size
      let butter-left test-butterproduction - butter-requirement
      ifelse butter-left > 0 [set test-buttersales (butter-left * global-butter-price)][set test-buttersales 0]

      let average-buttersales-percow test-buttersales * 4                                                                           ; this is the amount of milk she will produce over her life
      let exp-bullsales ( round sum (list random-float 1 random-float 1 random-float 1 random-float 1) ) * global-bull-sell-price   ; this is the number of bulls she will sell (0-4)
      report (average-buttersales-percow + exp-bullsales) / 10                                                                      ; this is the average income from sales over her life
    ]
  ][
    report 0
  ]
end

to calc-coffee-profit-test
  let coffee-requirement 0.2 * 12 * household-size ; according to https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/
  let coffee-left test-coffeeproduction - coffee-requirement
  ifelse coffee-left > 0 [set test-coffeesales coffee-left * global-coffee-price][set test-coffeesales 0]
  ; if who = test-hh [print (word "coffee left: " coffee-left)]
  ; if who = test-hh [print (word "test coffee sales: " test-coffeesales)]

  ; subtract establishment costs, production costs and fertiliser costs
  let establish-cost global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey and current-land-use = "coffee"] / 29 ; 29 = lifetime of plantation
  let production-cost 22309 * sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee"]                                              ; cost at peak production

  set test-coffeecost establish-cost + production-cost + calculate-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "coffee"]
  if any? in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "testing"] [set test-coffeecost test-coffeecost + calculate-cost-of-fertiliser-test]
  ; if who = test-hh [print (word "test coffee cost: " test-coffeecost)]
end

to calc-labour-test
  let maizelabour sum [field-size] of in-myfield-neighbors with [current-land-use = "maize"] * 92.8 ; person days per year
  let coffeelabour sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee"] * 132 ; person days per year
  let coffeeinitialisationlabour sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and color = grey] * 51 ; person days to establish plantation first year
  let vegetablelabour sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable"] * 92.8 + (92.8 * 0.15) ; person days per yeaer
  let cattlelabour (2.55 - 0.037 * count in-mycow-neighbors) * count in-mycow-neighbors ;
  let pasturelabour 0 ; find out later
  let testlaboursum maizelabour + coffeelabour + coffeeinitialisationlabour + vegetablelabour + cattlelabour + pasturelabour
  set leisure-expectation ((labourcap - testlaboursum)) / 56 ; leisure days per week
end

;; REPORTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-aspiration-ranks

  let n-unique table:length table:counts (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance))

  if n-unique < 3
  [
    set income-relativeimportance income-relativeimportance + random-float 1 - 0.5
    set leisure-relativeimportance leisure-relativeimportance + random-float 1 - 0.5
    set food-relativeimportance food-relativeimportance + random-float 1 - 0.5
  ]

  let income-rank position income-relativeimportance sort (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance)) + 1 ; will be 1 if most important, 2 if second most important and 3 if least important
  let leisure-rank position leisure-relativeimportance sort (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance)) + 1 ; will be 1 if most important, 2 if second most important and 3 if least important
  let food-rank position food-relativeimportance sort (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance)) + 1 ; will be 1 if most important, 2 if second most important and 3 if least important

  let ranks (list income-rank leisure-rank food-rank)
  report ranks
end

to-report drop-least-important
  ;print (word "1) household " who " is dropping least important aspirational dimension to find a better strategy" )

  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop replace-item (position 3 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 2 0
  let aspiration-id replace-item (position 2 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 1 0
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)

end

to-report drop-2ndleast-important
  ;print (word "2) household " who " is dropping 2nd least important aspirational dimension to find a better strategy" )

  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop replace-item (position 2 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report keep-only-most-important
  ;print (word "3) household " who " is keeping only the most important aspirational dimension to find a better strategy" )

  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop replace-item (position 3 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 2 0
  let aspiration-id replace-item (position 2 aspiration-drop) aspiration-drop 0 ; the second best (2nd) rank is replaced with 1----------------> 1 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report drop-most-important
  ;print (word "4) household " who " is dropping most important aspirational dimension to find a better strategy" )

  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop1 replace-item (position 1 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-drop replace-item (position 2 aspiration-drop1) aspiration-drop1 1 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report keep-only-2ndmost-important
  ;print (word "5) household " who " is keeping only the 2nd most important aspirational dimension to find a better strategy" )

  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop1 replace-item (position 1 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-drop replace-item (position 2 aspiration-drop1) aspiration-drop1 1 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 0 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report keep-only-least-important
  ;print (word "6) household " who " is keeping only the least important aspirational dimension to find a better strategy" )

  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop1 replace-item (position 1 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-drop replace-item (position 2 aspiration-drop1) aspiration-drop1 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report calculate-cost-of-fertiliser-test

  let fert-cost sum [(ifelse-value
    fertiliser = "testing moderate"  [(ifelse-value current-land-use = "maize" [88] current-land-use = "vegetable" [46] current-land-use = "coffee" [156] current-land-use = "pasture" [0]) * field-size * 25]
    fertiliser = "testing high"      [(ifelse-value current-land-use = "maize" [145] current-land-use = "vegetable" [56] current-land-use = "coffee" [321] current-land-use = "pasture" [0]) * field-size * 25]
    fertiliser = "testing very high" [(ifelse-value current-land-use = "maize" [209] current-land-use = "vegetable" [92] current-land-use = "coffee" [472] current-land-use = "pasture" [0]) * field-size * 25]
                                     [0]
    )] of fields with [member? "testing" fertiliser]

  report fert-cost
end

to-report calculate-cost-of-fertiliser [fields-of-interest]
  let fert-cost sum [(ifelse-value
    fertiliser = "low"       [0]
    fertiliser = "moderate"  [(ifelse-value current-land-use = "maize" [88] current-land-use = "vegetable" [46] current-land-use = "coffee" [156] current-land-use = "pasture" [0]) * field-size * 25]
    fertiliser = "high"      [(ifelse-value current-land-use = "maize" [145] current-land-use = "vegetable" [56] current-land-use = "coffee" [321] current-land-use = "pasture" [0]) * field-size * 25]
    fertiliser = "very high" [(ifelse-value current-land-use = "maize" [209] current-land-use = "vegetable" [92] current-land-use = "coffee" [472] current-land-use = "pasture" [0]) * field-size * 25]
                             [0]
  )] of fields-of-interest
  report fert-cost
end

; generic reporters
to-report take [n xs]
  report sublist xs 0 min list n (length xs)
end

to-report frequency [an-item a-list]
    report length (filter [ i -> i = an-item] a-list)
end

to-report get-delta [ outcome-mostpast outcome-mostrecent is-log?]

  let delta outcome-mostrecent - outcome-mostpast

  (ifelse
    is-log? = "log-experience"
    [let log-delta
      (ifelse-value
      delta > -1 and abs(delta) != delta [delta - 1.01] ; if between 0 and -1, reduce to below -1
      delta < 1 and abs(delta) = delta [delta + 1.01] ; if between 0 and 1, reduce to above 1
      [delta]
      )

      if log-delta = 0 [error (word "log-delta of " who " equals zero | outcome-mostrecent = " outcome-mostrecent " | outcome-mostpast = " outcome-mostpast " | delta = " delta)]

      report log-delta
    ]

    is-log? = "linear-experience"
    [ report delta ]

    is-log? = "change-rate-experience"
    [
      let rate-delta (outcome-mostrecent - outcome-mostpast) / ifelse-value outcome-mostpast = 0 [0.001] [outcome-mostpast]
      report rate-delta
    ]

    is-log? = "no-experience"
    [report 0]

    is-log? = "no-experience-alternative"
    [report 0]
  )

end

to-report get-experiences [delta-32 delta-21 is-log?]
  ifelse
    Aspiration-adaptation = "log-experience"[
    report (
      list
      ( log 10 (abs(delta-32)) * (delta-32 / abs(delta-32)) )
      ( log 10 (abs(delta-21)) * (delta-21 / abs(delta-21)) )
      0 )]
  [
   report (
     list
      (delta-32)
      (delta-21)
      0 )]
end

to-report calculate-ew [outcome-t3 outcome-t2 outcome-t1 is-log?]
  let my-forgetfullness forgetfullness

  ; get deltas
  let delta-32 get-delta outcome-t3 outcome-t2 Aspiration-adaptation
  let delta-21 get-delta outcome-t2 outcome-t1 Aspiration-adaptation

  ; get experiences
  let experiences get-experiences delta-32 delta-21 Aspiration-adaptation

  ; calculate
  let forgetfullness-adjusted-experiences (map * my-forgetfullness experiences)
  let ew sum forgetfullness-adjusted-experiences
  report ew
  ;set ew-income sum map [ i -> b-constant * i ] (map * my-forgetfullness experiences) ; the 1 here b-constant is some constant to make sure the ew stay within relatistic bounds
end

to check-67
  setup

  ask households with [who = 67][
      print (word
        "step: " ticks
        " | AO t-1: " precision income-outcome-t1start 4
        " | AO t-2: " precision income-outcome-t2 4
        " | AO t-3: " precision income-outcome-t3 4

        " | d AO 2-1: " precision (income-outcome-t2 - income-outcome-t1start) 4
        " | d AO 3-2: " precision (income-outcome-t3 - income-outcome-t2) 4

        " | AL t-1end: " precision income-threshold-t1end 4
        " | AL t-1start: " precision income-threshold-t1start 4
        " | AL t-2: " precision income-threshold-t2 4
        " | AL t-3: " precision income-threshold-t3 4

        " | ew: " precision ew-income 4
    )]

  repeat 21 [
    go
    ask households with [who = 67][
      print (word
        "step: " ticks
        " | AO t-1: " precision income-outcome-t1start 4
        " | AO t-2: " precision income-outcome-t2 4
        " | AO t-3: " precision income-outcome-t3 4

        " | d AO 2-1: " precision (income-outcome-t2 - income-outcome-t1start) 4
        " | d AO 3-2: " precision (income-outcome-t3 - income-outcome-t2) 4

        " | AL t-1end: " precision income-threshold-t1end 4
        " | AL t-1start: " precision income-threshold-t1start 4
        " | AL t-2: " precision income-threshold-t2 4
        " | AL t-3: " precision income-threshold-t3 4
        " | AL mean: " precision income-threshold-mean 4

        " | ew: " precision ew-income 4
    )]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Aesthetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to hide-fields
  ask fields [set hidden? true]
  ask myfields [set hidden? true]
end

to hide-cattle
  ask cattle [set hidden? true]
  ask mycows [set hidden? true]
  ask mycalves [set hidden? true]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to test-lu
  if (chance-maize + chance-pasture + chance-veg + chance-coffee > 1) [
    error ("The sum of the chances of different land uses cannot be > 1")
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialising households (done once) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test with importing and exporting agents
to clean-slate
  clear-all
  file-close-all ; Close any files open from last run
  reset-ticks
end

to generate-households
ask patches [set farmstead -1]
  ask n-of Number-of-households patches [
    sprout-households 1
    set farmstead 1
  ]

  ask households [
    set color white
    set pcolor grey
    set shape "house"
    ;set household-size random-poisson hhsize-lambda
  ]
end

; procedure to write some turtle properties to a file
to write-hhs-to-csv
  ; we use the `of` primitive to make a list of lists and then
  ; use the csv extension to write that list of lists to a file;
  csv:to-file "Netlogo_output/households.csv" [ (list who xcor ycor size color heading shape income-expectation) ] of households ;  why do I use income expectation here, and not income-outcome-t1end? Check in R
end
@#$#@#$#@
GRAPHICS-WINDOW
285
24
788
528
-1
-1
15.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
49
64
113
97
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
49
105
112
138
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
805
43
1033
209
Income t1
Income distribution (ETB/year)
Number of households
-100.0
100000.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true ";; set-plot-y-range 0 10\nset-plot-x-range -100000 100000\nset-histogram-num-bars 30\nset-plot-pen-color 0 \nplot 0\nhistogram [income-outcome-t1end] of households" "histogram [income-outcome-t1start] of households"

PLOT
804
218
1033
381
Maize consumption t1
Maize consumption distribution (kg per household per week)
NIL
0.0
50.0
0.0
10.0
true
false
"" ""
PENS
"default" 5.0 1 -16777216 true "histogram [food-outcome-t1end] of households" "histogram [food-outcome-t1start] of households"

BUTTON
120
105
180
138
Go once
Go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
28
231
149
291
Number-of-households
100.0
1
0
Number

TEXTBOX
25
202
175
222
Inputs
16
0.0
1

TEXTBOX
28
33
178
53
Commands
16
0.0
1

TEXTBOX
807
14
957
34
Monitoring
16
0.0
1

PLOT
805
565
1035
734
Cattle ownership
Cattle ownership (#)
Frequency (# hh)
0.0
10.0
0.0
10.0
true
false
"" "histogram [count mycow-neighbors] of households"
PENS
"default" 1.0 1 -16777216 true "histogram [count link-neighbors] of households" "histogram [count mycow-neighbors] of households"

INPUTBOX
26
304
105
364
remember-t3
0.5
1
0
Number

INPUTBOX
110
303
193
363
remember-t2
0.75
1
0
Number

INPUTBOX
198
303
277
363
remember-t1
0.9
1
0
Number

INPUTBOX
26
367
105
427
memory-time
3.0
1
0
Number

INPUTBOX
110
367
191
427
optimism
1.05
1
0
Number

INPUTBOX
15
570
97
630
chance-maize
0.6
1
0
Number

INPUTBOX
101
570
182
630
chance-pasture
0.2
1
0
Number

BUTTON
50
150
115
183
Hide fields
hide-fields
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1435
215
1720
380
Maize production
Years
kg per HH
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [maizeproduction] of households]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [maizeproduction] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [maizeproduction] of households]"
"25% quartile" 1.0 0 -1664597 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([maizeproduction] of households)]"
"75% quartile" 1.0 0 -8862290 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([maizeproduction] of households)]"

PLOT
1435
390
1720
550
Milk production
Years
Liter per HH
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [milkproduction] of households]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [milkproduction] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [milkproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([milkproduction] of households)]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([milkproduction] of households)]"

PLOT
806
749
1036
899
Household size
Household members (#)
Frequency (#hh)
0.0
20.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "histogram [household-size] of households" "histogram [household-size] of households"

PLOT
1765
215
2050
380
Maize profit
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [maizeprofit] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([maizeprofit] of households)]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [maizeprofit] of households]"
"Median" 1.0 0 -5987164 true "plot 0" "if ticks > 0 [plot median [maizeprofit] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([maizeprofit] of households)]"

PLOT
1435
560
1720
720
Butter production
Years
Kg per HH
0.0
10.0
0.0
2.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [butterproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([butterproduction] of households)]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [butterproduction] of households]"
"Median" 1.0 0 -5987164 true "plot 0" "if ticks > 0 [plot median [butterproduction] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([butterproduction] of households)]"

MONITOR
1640
675
1755
720
Butter producers (#)
count households with [butterproduction > 0]
0
1
11

MONITOR
1640
505
1740
550
Milk producers (#)
count households with [milkproduction > 0]
17
1
11

PLOT
1765
560
2050
720
Butter sales
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [buttersales] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([buttersales] of households)]"
"Mean" 1.0 0 -11053225 true "plot 0" "if ticks > 0 [plot mean [buttersales] of households]"
"Median" 1.0 0 -5987164 true "plot 0" "if ticks > 0 [plot median [buttersales] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([buttersales] of households)]"

MONITOR
1640
335
1750
380
Maize producers (#)
count households with [maizeproduction > 0]
17
1
11

MONITOR
935
635
1022
680
Oxen owners
count households with [oxen > 0]
17
1
11

MONITOR
936
589
1023
634
Cattle owners
count households with [herd > 0]
17
1
11

PLOT
1765
390
2050
545
Cattle sales
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [bullsales + oxensales] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([bullsales + oxensales] of households)]"
"Mean" 1.0 0 -11053225 true "plot 0" "if ticks > 0 [plot mean [bullsales + oxensales] of households]"
"Median" 1.0 0 -7500403 true "plot 0" "if ticks > 0 [plot median [bullsales + oxensales] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([bullsales + oxensales] of households)]"

MONITOR
1480
415
1537
460
Mean
mean [milkproduction] of households
0
1
11

MONITOR
1470
585
1527
630
Mean
mean [butterproduction] of households
0
1
11

MONITOR
1800
580
1857
625
Mean
mean [buttersales] of households
0
1
11

MONITOR
1805
415
1862
460
Mean
mean [oxensales + bullsales] of households
0
1
11

MONITOR
1815
240
1872
285
Mean
mean [maizeprofit] of households
0
1
11

MONITOR
1480
240
1537
285
Mean
mean [maizeproduction] of households
0
1
11

MONITOR
1970
330
2060
375
Maize sellers (#)
count households with [maizeprofit > 0]
17
1
11

MONITOR
1970
675
2065
720
Butter sellers (#)
count households with [butterprofit > 0]
17
1
11

MONITOR
1970
505
2072
550
Cattle sellers (#)
count households with [oxensales + bullsales > 0]
17
1
11

INPUTBOX
15
635
95
695
chance-veg
0.1
1
0
Number

BUTTON
120
150
180
183
Hide cattle
hide-cattle
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1435
45
1720
210
Vegetable prodution
Years
kg per HH
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [vegetableproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([vegetableproduction] of households)]"
"Mean" 1.0 0 -7500403 true "plot 0" "if ticks > 0 [plot mean [vegetableproduction] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [vegetableproduction] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([vegetableproduction] of households)]"

MONITOR
1470
65
1527
110
Mean
mean [vegetableproduction] of households
0
1
11

PLOT
1765
45
2055
210
Vegetable profit
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [vegetableprofit] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([vegetableprofit] of households)]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [vegetableprofit] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [vegetableprofit] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([vegetableprofit] of households)]"

MONITOR
1805
70
1862
115
Mean
mean [vegetableprofit] of households
0
1
11

MONITOR
1645
150
1745
195
Veg producers (#)
count households with [vegetableproduction > 0]
17
1
11

MONITOR
1975
165
2067
210
Veg sellers (#)
count households with [vegetableprofit > 0]
0
1
11

MONITOR
830
65
885
110
Mean
mean [income-outcome-t1end] of households
0
1
11

MONITOR
830
240
887
285
Mean
mean [food-outcome-t1end] of households
0
1
11

PLOT
2090
570
2355
735
Mean income
Year
ETB per HH
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Min" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [income-outcome-t1end] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([income-outcome-t1end] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [income-outcome-t1end] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [income-outcome-t1end] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([income-outcome-t1end] of households)]"

PLOT
2090
220
2355
385
Mean maize consumption
Year
kg per (HH*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [food-outcome-t1end] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([food-outcome-t1end] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [food-outcome-t1end] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [food-outcome-t1end] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot max [food-outcome-t1end] of households]"

TEXTBOX
2095
155
2245
173
Aspirational dimensions
12
0.0
1

MONITOR
2275
680
2355
725
Negative HH's
count households with [income-outcome-t1end < 0.001]
0
1
11

MONITOR
2135
590
2192
635
Mean
mean [income-outcome-t1end] of households
0
1
11

MONITOR
2120
240
2177
285
Mean
mean [food-outcome-t1end] of households
0
1
11

TEXTBOX
2095
195
2245
213
Aspirational outcomes
11
0.0
1

TEXTBOX
2370
195
2520
213
Aspirational thresholds
11
0.0
1

PLOT
2370
220
2635
385
Mean food threshold
Years
kg consumed (hh*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [food-threshold-mean] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([food-threshold-mean] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [food-threshold-mean] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [food-threshold-mean] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([food-threshold-mean] of households)]"

PLOT
2370
570
2635
735
Mean income threshold
Years
Birr (HH*year)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [income-threshold-mean] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([income-threshold-mean] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [income-threshold-mean] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [income-threshold-mean] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([income-threshold-mean] of households)]"

PLOT
2025
1100
2570
1260
Income of two random households
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Outcome HH1" 1.0 0 -6565750 true "" "if ticks > 0 [plot mean [income-outcome-t1end] of households with [who = 1]]"
"Aspiration HH1" 1.0 0 -10899396 true "" "if ticks > 0 [plot mean [income-threshold-t1start] of households with [who = 1]]"
"Outcome HH2" 1.0 0 -8275240 true "" "if ticks > 0 [plot mean [income-outcome-t1end] of households with [who = 2]]"
"Aspiration HH2" 1.0 0 -13791810 true "" "if ticks > 0 [plot mean [income-threshold-t1start] of households with [who = 2]]"

PLOT
2025
745
2570
905
Maize consumption of two random households
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Outcome HH1" 1.0 0 -6565750 true "" "if ticks > 0 [plot mean [food-outcome-t1end] of households with [who = 1]]"
"Aspiration HH1" 1.0 0 -10899396 true "" "if ticks > 0 [plot mean [food-threshold-t1start] of households with [who = 1]]"
"Outcome HH2" 1.0 0 -8275240 true "" "if ticks > 0 [plot mean [food-outcome-t1end] of households with [who = 2]]"
"Aspiration HH2" 1.0 0 -13791810 true "" "if ticks > 0 [plot mean [food-threshold-t1start] of households with [who = 1]]"

MONITOR
2560
330
2630
375
Mean gap
mean [food-threshold-mean - food-outcome-t1end] of households
0
1
11

MONITOR
2565
680
2630
725
Mean gap
mean [income-threshold-mean - income-outcome-t1end] of households
0
1
11

MONITOR
2490
1195
2557
1240
Gap HH1
mean [income-threshold-mean - income-outcome-t1end] of households with [who = 1]
0
1
11

MONITOR
2490
835
2557
880
Gap HH1
mean [food-threshold-mean - food-outcome-t1end] of households with [who = 1]
0
1
11

MONITOR
2480
300
2537
345
Mean
mean [food-threshold-mean] of households
0
1
11

MONITOR
2485
670
2542
715
Mean
mean [income-threshold-mean] of households
0
1
11

MONITOR
2260
140
2352
185
Satisficed HH's
count households with [satisficed = \"yes\"]
0
1
11

PLOT
2370
40
2635
190
Satisficed HH's
Year
Count
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"satisficed" 1.0 0 -16777216 true "" "plot count households with [satisficed = \"yes\"]"
"could not find" 1.0 0 -2674135 true "" "plot count households with [color = red]"

MONITOR
560
625
775
670
...could not find a satisficing strategy
count households with [color = red]
0
1
11

MONITOR
560
675
775
720
...found a satisificing strategy
count households with [color = 67]
0
1
11

MONITOR
560
575
777
620
 ...searched for a satisficing strategy
count households with [color = 67] + count households with [color = red]
0
1
11

TEXTBOX
590
550
740
568
Households that...
12
0.0
1

MONITOR
1365
1090
1485
1135
wealth of bottom 10%
sum sublist sort [income-outcome-t1start] of households 0 ceiling (Number-of-households * 0.10) / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1365
1140
1485
1185
wealth of bottom 25%
sum sublist sort [income-outcome-t1start] of households 0 ceiling (Number-of-households * 0.25) / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1365
1190
1485
1235
wealth of bottom 50%
sum sublist sort [income-outcome-t1start] of households 0 ceiling (Number-of-households * 0.50) / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1515
1190
1635
1235
wealth of top 50%
sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.50) Number-of-households / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1515
1140
1632
1185
wealth of top 25%
sum sublist sort [income-outcome-t1start] of households round (Number-of-households * 0.75) Number-of-households / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1515
1090
1632
1135
wealth of top 10%
sum sublist sort [income-outcome-t1start] of households round (Number-of-households * 0.90) Number-of-households / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1660
1090
1730
1135
GINI index
100 - gini
0
1
11

TEXTBOX
1745
1100
1895
1126
Range [0, 100] goes from very equal to extremely inequal
11
0.0
1

INPUTBOX
100
635
180
695
chance-coffee
0.1
1
0
Number

PLOT
285
540
545
725
Land use
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"maize" 1.0 0 -1184463 true "plot count fields with [current-land-use = \"maize\"]" "plot count fields with [current-land-use = \"maize\"]"
"pasture" 1.0 0 -10899396 true "plot count fields with [current-land-use = \"pasture\"]" "plot count fields with [current-land-use = \"pasture\"]"
"vegetable" 1.0 0 -955883 true "plot count fields with [current-land-use = \"vegetable\"]" "plot count fields with [current-land-use = \"vegetable\"]"
"coffee" 1.0 0 -6459832 true "plot count fields with [current-land-use = \"coffee\"]" "plot count fields with [current-land-use = \"coffee\"]"
"fallow" 1.0 0 -7500403 true "plot count fields with [current-land-use = \"fallow\"]" "plot count fields with [current-land-use = \"fallow\"]"

PLOT
290
745
545
895
Non-maize fields
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"pastures" 1.0 0 -10899396 true "" "plot count fields with [current-land-use = \"pasture\"]"
"vegetable" 1.0 0 -955883 true "" "plot count fields with [current-land-use = \"vegetable\"]"
"coffee" 1.0 0 -6459832 true "" "plot count fields with [current-land-use = \"coffee\"]"
"fallow" 1.0 0 -7500403 true "" "plot count fields with [current-land-use = \"fallow\"]"

PLOT
1375
910
1660
1070
Coffee production
Years
kg per HH
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [coffeeproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([coffeeproduction] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [coffeeproduction] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [coffeeproduction] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([coffeeproduction] of households)]"

MONITOR
1410
930
1467
975
Mean
mean [coffeeproduction] of households
0
1
11

MONITOR
1580
1025
1695
1070
Coffee producers (#)
count households with [coffeeland > 0]
0
1
11

PLOT
1705
910
1990
1070
Coffee profit
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [coffeeprofit] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([coffeeprofit] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [coffeeprofit] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [coffeeprofit] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([coffeeprofit] of households)]"

MONITOR
1910
1025
2010
1070
Coffee sellers (#)
count households with [coffeeprofit > 0]
0
1
11

MONITOR
1755
940
1812
985
Mean
mean [coffeeprofit] of households
0
1
11

PLOT
806
924
1036
1074
Savings
Savings (ETB)
Frequency (#hh)
-10000.0
10000.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true ";; set-plot-y-range 0 10\nset-plot-x-range -1000000 1000000\nset-histogram-num-bars 30\nset-plot-pen-color 0 \nplot 0\nhistogram [savings] of households" "histogram [savings] of households"

MONITOR
831
944
896
989
Mean
mean [savings] of households
0
1
11

MONITOR
961
944
1026
989
Median
median [savings] of households
0
1
11

PLOT
805
390
1030
555
Leisure t1
person days per hh per week
NIL
-10.0
50.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "set-histogram-num-bars 30\nset-plot-pen-color 0 " "histogram [leisure-outcome-t1end] of households"

PLOT
2090
395
2355
560
Mean leisure 
Year
person days (HH*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [leisure-outcome-t1end] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([leisure-outcome-t1end] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [leisure-outcome-t1end] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [leisure-outcome-t1end] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([leisure-outcome-t1end] of households)]"

PLOT
2370
395
2635
560
Mean leisure threshold
Years
person days (HH*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [leisure-threshold-mean] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([leisure-threshold-mean] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [leisure-threshold-mean] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [leisure-threshold-mean] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([leisure-threshold-mean] of households)]"

MONITOR
2120
415
2177
460
Mean
mean [leisure-outcome-t1end] of households
1
1
11

PLOT
2025
920
2570
1085
Leisure of two random households
Year
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Outcome HH1" 1.0 0 -5509967 true "" "if ticks > 0 [plot mean [leisure-outcome-t1end] of households with [who = 1]]"
"Aspiration HH1" 1.0 0 -14439633 true "" "if ticks > 0 [plot mean [leisure-threshold-t1start] of households with [who = 1]]"
"Outcome HH2" 1.0 0 -6759204 true "" "if ticks > 0 [plot mean [leisure-outcome-t1end] of households with [who = 2]]"
"Threshold HH2" 1.0 0 -13403783 true "" "if ticks > 0 [plot mean [leisure-threshold-t1start] of households with [who = 2]]"

MONITOR
2475
490
2532
535
Mean
mean [leisure-threshold-mean] of households
1
1
11

MONITOR
2405
1020
2482
1065
threshold-t1
mean [leisure-threshold-t1start] of households with [who = 1]
2
1
11

MONITOR
2325
1020
2402
1065
outcome-t1
mean [leisure-outcome-t1end] of households with [who = 1]
2
1
11

MONITOR
2485
1020
2562
1065
threshold-m
mean [leisure-threshold-mean] of households with [who = 1]
2
1
11

SWITCH
165
255
267
288
fix-seed?
fix-seed?
0
1
-1000

BUTTON
200
475
275
510
generate hhs
generate-households
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
437
275
470
clean slate
clean-slate
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
515
275
548
hhs to csv
write-hhs-to-csv
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
590
270
623
hhs from csv
read-households-from-csv
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
555
270
588
let hhs die
ask households [die]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
560
745
795
895
dissatisficed hh that...
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"... found" 1.0 0 -13840069 true "" "plot count households with [color = green - 2]"
"... didn't" 1.0 0 -2674135 true "" "plot count households with [color = red]"
"... searched" 1.0 0 -7500403 true "" "plot count households with [color = red] + count households with [color = green - 2]"

CHOOSER
600
35
807
80
Aspiration-adaptation
Aspiration-adaptation
"no-experience" "no-experience-alternative" "change-rate-experience" "log-experience" "linear-experience"
0

CHOOSER
600
85
785
130
Strategy-order
Strategy-order
"random" "defined-by-similarity"
1

INPUTBOX
705
300
800
360
b-log
0.02
1
0
Number

INPUTBOX
700
495
795
555
b-lin
2.0E-5
1
0
Number

INPUTBOX
702
365
797
425
b-rate
1.45
1
0
Number

INPUTBOX
700
430
795
490
b-no
0.45
1
0
Number

SWITCH
165
210
265
243
write-file?
write-file?
0
1
-1000

BUTTON
195
105
262
138
Go 10x
repeat 10 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
20
430
200
471
memory time does not (yet) have any impact on how the model runs
11
0.0
1

TEXTBOX
20
530
180
565
initial ATs = AOs + AOs * initial-aspiration-factor
11
0.0
1

MONITOR
960
410
1017
455
mean
mean [leisure-outcome-t1start] of households
0
1
11

TEXTBOX
200
375
350
416
pessimism: < 1\nneutral: = 1\noptimism: > 1\n
11
0.0
1

BUTTON
175
60
242
93
Go 20x
repeat 20 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
2560
505
2627
550
Mean gap
mean [leisure-threshold-mean - leisure-outcome-t1end] of households
1
1
11

PLOT
1765
735
2050
895
Profit from cattle and butter
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [bullsales + oxensales + buttersales] of households]"
"25% quartile" 1.0 0 -1664597 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([bullsales + oxensales + buttersales] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [bullsales + oxensales + buttersales] of households]"
"Median" 1.0 0 -7500403 true "" "if ticks > 0 [plot median [bullsales + oxensales + buttersales] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([bullsales + oxensales + buttersales] of households)]"

MONITOR
1725
755
1782
800
Mean
mean [bullsales + oxensales + buttersales] of households
0
1
11

BUTTON
165
15
232
48
Go 50x
repeat 50 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
985
15
1052
48
Go 23x
repeat 23 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1075
30
1190
75
chosen
word [chosen-strategy] of households with [who = test-hh]
17
1
11

MONITOR
1180
100
1265
145
Herd
word [count in-mycow-neighbors] of households with [who = test-hh]
17
1
11

MONITOR
1185
245
1270
290
All costs
word [round(costs-of-production)] of households with [who = test-hh]
0
1
11

MONITOR
1185
300
1270
345
Cow cost
word [cowcost] of households with [who = test-hh]
17
1
11

MONITOR
1185
350
1270
395
Feed cost
word [round(supplementaryfeedcost)] of households with [who = test-hh]
0
1
11

TEXTBOX
1085
15
1235
33
Test household
11
0.0
1

TEXTBOX
1080
85
1230
103
Expected
11
0.0
1

TEXTBOX
1190
85
1340
103
Actual
11
0.0
1

MONITOR
1075
300
1160
345
Cow cost
word [round(test-cowcost)] of households with [who = test-hh]
0
1
11

MONITOR
1075
350
1160
395
Feed cost
word [round(test-feedcost)] of households with [who = test-hh]
0
1
11

MONITOR
1073
100
1158
145
Herd
word [test-herd] of households with [who = test-hh]
0
1
11

MONITOR
1075
400
1160
445
Maize cost
[round(test-maizecost)] of households with [who = test-hh]
0
1
11

MONITOR
1077
450
1157
495
Veg cost
[round(test-vegetablecost)] of households with [who = test-hh]
0
1
11

MONITOR
1080
500
1157
545
Coffee cost
[round(test-coffeecost)] of households with [who = test-hh]
0
1
11

MONITOR
1185
400
1270
445
Maize cost
[round(maizecost)] of households with [who = test-hh]
0
1
11

MONITOR
1185
450
1270
495
Veg cost
[round(vegetablecost)] of households with [who = test-hh]
0
1
11

MONITOR
1185
500
1262
545
Coffee cost
[round(coffeecost)] of households with [who = test-hh]
0
1
11

MONITOR
1075
245
1160
290
All costs
[round(test-maizecost + test-vegetablecost + test-cowcost + test-feedcost + test-coffeecost)] of households with [who = test-hh]
0
1
11

MONITOR
1075
550
1147
595
Maize sales
[round(test-maizesales)] of households with [who = test-hh]
0
1
11

MONITOR
1185
550
1257
595
Maize sales
[round(maizesales)] of households with [who = test-hh]
0
1
11

MONITOR
1075
600
1145
645
Veg sales
[round test-vegetablesales] of households with [who = test-hh]
0
1
11

MONITOR
1185
600
1260
645
Veg sales
[round vegetablesales] of households with [who = test-hh]
0
1
11

MONITOR
1070
650
1147
695
Butter sales
[round test-buttersales] of households with [who = test-hh]
0
1
11

MONITOR
1185
650
1262
695
Butter sales
[round buttersales] of households with [who = test-hh]
0
1
11

MONITOR
1075
700
1145
745
Bull sales
[round test-bullsales] of households with [who = test-hh]
0
1
11

MONITOR
1185
700
1260
745
Bull sales
[round bullsales ] of households with [who = test-hh]
0
1
11

MONITOR
1065
750
1147
795
Coffee sales
[round test-coffeesales] of households with [who = test-hh]
0
1
11

MONITOR
1185
750
1267
795
Coffee sales
[round coffeesales] of households with [who = test-hh]
0
1
11

INPUTBOX
1200
15
1250
75
test-hh
70.0
1
0
Number

MONITOR
1075
805
1147
850
Oxen sales
[round test-oxensales] of households with [who = test-hh]
17
1
11

MONITOR
1185
805
1260
850
Oxen sales
[round oxensales] of households with [who = test-hh]
17
1
11

MONITOR
1285
350
1347
395
Adopted?
word [adopted-feed?] of households with [who = test-hh]
17
1
11

MONITOR
1185
195
1242
240
All sales
word [round(earnings-from-sales)] of households with [who = test-hh]
17
1
11

MONITOR
1080
195
1137
240
All sales
[round(test-maizesales + test-vegetablesales + test-buttersales + test-bullsales + test-oxensales + test-coffeesales)] of households with [who = test-hh]
17
1
11

MONITOR
1185
150
1242
195
Income
[round income-outcome-t1end] of households with [who = test-hh]
17
1
11

MONITOR
1080
150
1137
195
Income
[round income-expectation] of households with [who = test-hh]
17
1
11

MONITOR
860
590
932
635
Cattle total
count cattle
0
1
11

MONITOR
1070
855
1140
900
Cow sales
[round test-cowsales] of households with [who = test-hh]
0
1
11

MONITOR
1185
855
1252
900
Cow sales
[round oxensales] of households with [who = test-hh]
17
1
11

MONITOR
1275
100
1350
145
savings
[round(savings)] of households with [who = test-hh]
17
1
11

MONITOR
1265
150
1332
195
maizeland
[precision maizeland 2] of households with [who = test-hh]
17
1
11

MONITOR
1280
255
1337
300
Pasture
[precision privatepasture 2] of households with [who = test-hh]
17
1
11

MONITOR
1265
200
1357
245
maizepurchase
[round(maizepurchase)] of households with [who = test-hh]
17
1
11

MONITOR
1357
100
1432
145
coffeeland
[precision coffeeland 2] of households with [who = test-hh]
17
1
11

MONITOR
1340
150
1432
195
vegetableland
[precision vegetableland 2] of households with [who = test-hh]
17
1
11

MONITOR
1350
255
1412
300
Feedprod
[round feedproduction] of households with [who = test-hh]
17
1
11

MONITOR
1350
305
1412
350
Capacity
[floor (((feedproduction * 1000) / 365 )  / 3.15)] of households with [who = test-hh]
17
1
11

MONITOR
1260
15
1795
60
Options
[known-strategies] of households with [who = test-hh]
17
1
11

MONITOR
1300
420
1357
465
hh-size
[household-size] of households with [who = test-hh]
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

house two story
false
0
Polygon -7500403 true true 2 180 227 180 152 150 32 150
Rectangle -7500403 true true 270 75 285 255
Rectangle -7500403 true true 75 135 270 255
Rectangle -16777216 true false 124 195 187 256
Rectangle -16777216 true false 210 195 255 240
Rectangle -16777216 true false 90 150 135 180
Rectangle -16777216 true false 210 150 255 180
Line -16777216 false 270 135 270 255
Rectangle -7500403 true true 15 180 75 255
Polygon -7500403 true true 60 135 285 135 240 90 105 90
Line -16777216 false 75 135 75 180
Rectangle -16777216 true false 30 195 93 240
Line -16777216 false 60 135 285 135
Line -16777216 false 255 105 285 135
Line -16777216 false 0 180 75 180
Line -7500403 true 60 195 60 240
Line -7500403 true 154 195 154 255

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="explore-feed-milk" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20"/>
    <metric>[feedproduction] of households</metric>
    <metric>[maizeproduction] of households</metric>
    <metric>[coffeeproduction] of households</metric>
    <metric>[feed-quantity] of cattle</metric>
    <metric>[chosen-strategy] of households</metric>
    <metric>[supplementary-feed] of cattle</metric>
    <metric>[milkyield] of cattle</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-alpha">
      <value value="1.1123473"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-mu">
      <value value="5538"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-lambda">
      <value value="0.1128911"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ask households with [count in-mycow-neighbors &gt; 0][show known-strategies show feedproduction show count in-mycow-neighbors show chosen-strategy]</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-alpha">
      <value value="1.1123473"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-mu">
      <value value="5538"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-lambda">
      <value value="0.1128911"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's and formulas" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-rate" first="0" step="2" last="0.1"/>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-log" first="0" step="0.0025" last="0.05"/>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-lin" first="0" step="5.0E-7" last="1.0E-5"/>
  </experiment>
  <experiment name="Aspiration b's no experience" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20"/>
    <metric>[income-threshold-mean] of households</metric>
    <metric>[income-threshold-t1end] of households</metric>
    <metric>[income-threshold-t1start] of households</metric>
    <metric>[income-threshold-t2] of households</metric>
    <metric>[income-threshold-t3] of households</metric>
    <metric>[food-threshold-mean] of households</metric>
    <metric>[food-threshold-t1end] of households</metric>
    <metric>[food-threshold-t1start] of households</metric>
    <metric>[food-threshold-t2] of households</metric>
    <metric>[food-threshold-t3] of households</metric>
    <metric>[leisure-threshold-mean] of households</metric>
    <metric>[leisure-threshold-t1end] of households</metric>
    <metric>[leisure-threshold-t1start] of households</metric>
    <metric>[leisure-threshold-t2] of households</metric>
    <metric>[leisure-threshold-t3] of households</metric>
    <metric>[income-outcome-t3] of households</metric>
    <metric>[income-outcome-t2] of households</metric>
    <metric>[income-outcome-t1start] of households</metric>
    <metric>[income-outcome-t1end] of households</metric>
    <metric>[food-outcome-t3] of households</metric>
    <metric>[food-outcome-t2] of households</metric>
    <metric>[food-outcome-t1start] of households</metric>
    <metric>[food-outcome-t1end] of households</metric>
    <metric>[leisure-outcome-t3] of households</metric>
    <metric>[leisure-outcome-t2] of households</metric>
    <metric>[leisure-outcome-t1start] of households</metric>
    <metric>[leisure-outcome-t1end] of households</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's change rate bno0.2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15"/>
    <metric>[income-threshold-mean] of households</metric>
    <metric>[income-threshold-t1end] of households</metric>
    <metric>[income-threshold-t1start] of households</metric>
    <metric>[income-threshold-t2] of households</metric>
    <metric>[income-threshold-t3] of households</metric>
    <metric>[food-threshold-mean] of households</metric>
    <metric>[food-threshold-t1end] of households</metric>
    <metric>[food-threshold-t1start] of households</metric>
    <metric>[food-threshold-t2] of households</metric>
    <metric>[food-threshold-t3] of households</metric>
    <metric>[leisure-threshold-mean] of households</metric>
    <metric>[leisure-threshold-t1end] of households</metric>
    <metric>[leisure-threshold-t1start] of households</metric>
    <metric>[leisure-threshold-t2] of households</metric>
    <metric>[leisure-threshold-t3] of households</metric>
    <metric>[income-outcome-t3] of households</metric>
    <metric>[income-outcome-t2] of households</metric>
    <metric>[income-outcome-t1start] of households</metric>
    <metric>[income-outcome-t1end] of households</metric>
    <metric>[food-outcome-t3] of households</metric>
    <metric>[food-outcome-t2] of households</metric>
    <metric>[food-outcome-t1start] of households</metric>
    <metric>[food-outcome-t1end] of households</metric>
    <metric>[leisure-outcome-t3] of households</metric>
    <metric>[leisure-outcome-t2] of households</metric>
    <metric>[leisure-outcome-t1start] of households</metric>
    <metric>[leisure-outcome-t1end] of households</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-rate" first="0" step="0.1" last="2"/>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;change-rate-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's change-rate" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0.1" step="0.01" last="0.2"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-rate" first="0" step="0.01" last="0.1"/>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;change-rate-experience&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="optimism" first="0.9" step="0.05" last="1.1"/>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-aspiration-factor">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's no-exp calibrate" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="optimism" first="0.8" step="0.05" last="1.5"/>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's no-exp calibrate 2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.125"/>
      <value value="0.175"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="optimism" first="0.8" step="0.1" last="1.5"/>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
