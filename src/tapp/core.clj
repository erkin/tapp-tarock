(ns tapp.core
  (:require [clojure.pprint :refer [pprint]])
  (:require [tapp.util :refer :all])
  (:gen-class))

(def players [:forehand :middlehand :rearhand])

(def red-ranks [4 3 2 1 :jack :rider :queen :king])
(def black-ranks [7 8 9 10 :jack :rider :queen :king])

(def the-deck
  (concat
   (for [rank red-ranks
         suit [:hearts :diamonds]]
     {:rank rank :suit suit})
   (for [rank black-ranks
         suit [:clubs :spades]]
     {:rank rank :suit suit})
   (for [rank (range 1 23)]
     {:rank rank :suit :trump})))

(defn card>?
  "Compare the ranks of two cards `a` and `b` of the same suit."
  [a b]
  (let [ra (:rank a)
        rb (:rank b)]
    (case (:suit a)
      :trump (> ra rb)
      (:hearts :diamonds) (listed>? ra rb red-ranks)
      (:clubs :spades) (listed>? ra rb black-ranks))))

(defn filter-hand
  "Get cards in `cards` of the suit `suit`."
  [suit cards]
  (filter #(= suit (:suit %)) cards))

(defn playable-cards
  "Get cards from `hand` that can be legally played to `led-card`."
  [led-card hand]
  ;; Was a card led?
  (if led-card
    ;; Do we have a card of the same suit?
    (if-let [playable (not-empty (filter-hand (:suit led-card) hand))]
      playable
      ;; Do we have a trump at least?
      (if-let [playable (not-empty (filter-hand (:suit :trump) hand))]
        playable
        ;; Discard any card.
        hand))
    ;; We're leading.
    hand))

(defn evaluate-points
  "Convert `cards` won by player into numeric score.
  The length of `cards` must be a multiple of three."
  [cards]
  (defn get-point
    "Return the value of `card`."
    [card]
    (let [rank (:rank card)]
      (if (= :trump (:suit card))
        ;; Trulls are worth five points.
        (if (.contains [1 21 22] rank)
          5
          1)
        ;; Pips and other tarots are worth one point.
        (if (number? rank)
          1
          (rank {:jack 2 :rider 3 :queen 4 :king 5})))))
  (defn sum-trick
    "Sum the values of `three-cards` and subtract two."
    [three-cards]
    (- (reduce + (map get-point three-cards))
       2))
  ;; Split the cards into tricks of three cards.
  (reduce + (map sum-trick (partition 3 cards))))

(defn update-score
  "Shorthand (im)mutator for `state` to evaluate the
  value of `cards` won by `winner` in a trick."
  [state winner cards]
  (update state winner + (evaluate-points cards)))

(defn winner
  "Return one of the three card on the `ground` that won the trick."
  [ground]
  ;; If a trump was played, the highest trump wins.
  (if-let [trumps (not-empty (filter-hand :trump ground))]
    (best card>? trumps)
    ;; Otherwise, the highest card of the suit led wins.
    (let [led-suit (:suit (first ground))]
      (best card>? (filter-hand led-suit ground)))))

(def player-order (cycle [:forehand :middlehand :rearhand]))

(defn next-player
  "Return the player to play after `player`.
  (Say that ten times fast.)"
  [player]
  (nth player-order (inc (.indexOf player-order player))))

(defn play-card
  "Main game logic - loop over `state` until cards run out in `:hands`."
  [state]
  (let [current-player (:turn state)
        hand (current-player (:hands state))
        ground (:ground state)]
    (cond
      ;; Three cards have already been played. Let's find the winner.
      ;; Currently, :turn refers to the player who led the trick.
      (= (count ground) 3)
      (let [winning-card (winner ground)
            winning-player (nth (drop-while (partial not= (:turn state))
                                            player-order)
                                (.indexOf ground winning-card))]
        (-> state
            ;; The winner takes the points.
            (update-score (if (= winning-player (:player state))
                            :player-score
                            :defender-score)
                          ground)
            ;; Clear the ground.
            (assoc :ground nil)
            ;; The player that wins the trick leads the next one.
            (assoc :turn winning-player)
            play-card))
      ;; If there are no cards left in hand, the game is over.
      (empty? hand)
      state
      ;; Play the first legal card in the hand.
      ;; TODO: Add actual AI logic.
      :else
      (let [played (first (playable-cards (first ground) hand))]
        (-> state
            ;; Remove the played card from the player's hand.
            (update-in [:hands current-player] remove-from played)
            ;; Put the card on the ground.
            (update :ground conj played)
            ;; And the discard pile, meaning this card is no longer in a hand.
            (update :discard conj played)
            ;; End turn.
            (update :turn next-player)
            play-card)))))

(defn should-bid?
  "Guess if `hand` is good enough for a `bid` game.
  A simple heuristic that counts kings and trumps.
  Trumps ranking >16 are counted twice."
  [bid hand]
  (let [trumps (filter-hand :trump hand)
        hand-value (+ (count trumps)
                      (count (filter #(>    16 (:rank %)) trumps))
                      (count (filter #(= :king (:rank %)) hand)))]
    (> hand-value (bid {:small 11 :under 12 :over 13 :solo 14}))))

(defn bid
  "Auction logic here - loop over `state` until a player wins a bid
  or all players pass."
  [state]
  (def bids [nil :small :under :over :solo])
  (let [possible-bids (rest (drop-while (partial not= (:game state)) bids))]
    (cond
      ;; The players reached the highest bid. The game must begin.
      (empty? possible-bids)
      state
      ;; Someone bid and the next two players passed.
      (and (some? (:game state))
           (= 2 (:passes state)))
      state
      ;; Everyone passed!
      (= 3 (:passes state))
      (assoc state :game :passed)
      ;; Shall we play?
      (should-bid? (first possible-bids) (get-in state [:hands (:turn state)]))
      (-> state
          (assoc :game (first possible-bids))
          (assoc :player (:turn state))
          (update :turn next-player)
          bid)
      ;; Nope.
      :else
      (-> state
          (update :passes inc)
          (update :turn next-player)
          bid))))

(defn auction
  "Take the initial state `st`, put it through `bid` and incorporate
  the `talon` into the state according to the result of the auction."
  [st talon]
  (defn get-hand
    "Shorthand accessor to get the `:player`'s hand in `state`."
    [state]
    (get-in state [:hands (:player state)]))
  (defn pickup-cards
    "Incorporate `cards` into the `:player`'s hand in `state`."
    [state cards]
    (update-in state [:hands (:player state)] #(shuffle (concat % cards))))
  (defn drop-cards
    "Discard `n` cards from the `:player`'s hand in `state`."
    [state n]
    (update-in state [:hands (:player state)] (partial drop n)))
  (let [state (bid st)
        [upper lower] (split-at 3 talon)]
    (case (:game state)
      :small
      (as-> state $
        ;; Add the talon to your hand.
        (pickup-cards $ talon)
        ;; Put down six cards that count to your score.
        (update-score $ :player-score (take 6 (get-hand $)))
        (drop-cards $ 6))
      :over
      (as-> state $
        ;; Add the upper half of the talon to your hand.
        (pickup-cards $ upper)
        ;; Put down three cards that count to your score.
        (update-score $ :player-score (take 3 (get-hand $)))
        (drop-cards $ 3)
        ;; The lower half of the talon goes straight to your score.
        (update-score $ :player-score lower))
      :under
      (as-> state $
        ;; Add the lower half of the talon to your hand.
        (pickup-cards $ lower)
        ;; Put down three cards that count to your score.
        (update-score $ :player-score (take 3 (get-hand $)))
        (drop-cards $ 3)
        ;; The upper half of the talon goes straight to the defenders' score.
        (update-score $ :defender-score upper))
      :solo
      ;; The entire talon goes straight to the defenders' score.
      (update-score state :defender-score talon)
      ;; Everyone passed.
      :passed
      state)))

(defn begin-game
  "Prepare the game state by shuffling and splitting `deck` of cards.
  Put the `state` through `auction`, then send to `play-card`,
  finally clean it up and return."
  [deck]
  ;; Deal 16 cards to all players, the remaining 6 go to the talon.
  (let [[forehand middlehand rearhand talon] (partition-all 16 (shuffle deck))]
    (as-> {:hands {:forehand forehand :middlehand middlehand :rearhand rearhand}
           :player nil :game nil :passes 0
           :turn :forehand :ground nil :discard nil
           :player-score 0 :defender-score 0}
        state
      (auction state talon)
      ;; Forehand begins playing after the auction.
      (assoc state :turn :forehand)
      (if-not (= :passed (:game state))
        (play-card state)
        state)
      ;; Excise slots useless for post-game information.
      (dissoc state :discard :ground :hands :passes :turn)
      (assoc state :won? (> (:player-score state)
                            (:defender-score state))))))

(defn -main [& args]
  (pprint (begin-game the-deck)))
