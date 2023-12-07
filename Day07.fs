module Day07

open System

// Represents the type of hand that arise from the combinations of cards in a
// hand in ascending order of value
type HandType =
    | HighCard = 0
    | OnePair = 1
    | TwoPair = 2
    | ThreeOfAKind = 3
    | FullHouse = 4
    | FourOfAKind = 5
    | FiveOfAKind = 6

// Represents a hand during a round of the game
type Hand =
    { Cards: string
      Optimized: string
      Type: HandType
      Strength: int }

// Represents a round of the game
type Round = { Hand: Hand; Bid: int }

// Get the type of a hand
let getHandType hand =
    let histogram =
        hand
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.sortDescending
        |> Array.ofSeq

    match Array.length histogram with
    | 1 -> HandType.FiveOfAKind
    | 2 ->
        match histogram[0] with
        | 4 -> HandType.FourOfAKind
        | _ -> HandType.FullHouse
    | 3 ->
        match histogram[0] with
        | 3 -> HandType.ThreeOfAKind
        | _ -> HandType.TwoPair
    | 4 -> HandType.OnePair
    | 5 -> HandType.HighCard
    | _ -> failwith "something wrong"

// Optimize hand cards by replacing joker ('J') with every other card and
// choosing the hand that has the type with the highest value
let optimizeHandCards (cards: string) (handCards: string) =
    cards
    |> Seq.filter (fun c -> c <> 'J')
    |> Seq.map (fun card -> handCards.Replace('J', card))
    |> Seq.sortWith (fun a b -> compare (getHandType a) (getHandType b))
    |> Seq.last

// Get the strength of a hand by converting it to a number where each card (in
// sequence) is n times more valuable than the next (when n is the number of
// possible cards)
let getHandStrength (cardValues: Map<char, int>) handCards =
    let cardCount = Map.count cardValues
    handCards |> Seq.fold (fun acc c -> acc * cardCount + cardValues[c]) 0

// Parse a round (hand and bid) from a line of text and calculate it's properties
let parseRound (cards: string) (wildCards: bool) (line: string) =
    let split = line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    let cardValues = cards |> Seq.mapi (fun i c -> (c, i)) |> Map.ofSeq

    let handCards =
        if wildCards then
            optimizeHandCards cards split[0]
        else
            split[0]

    { Hand =
        { Cards = split[0]
          Optimized = handCards
          Type = getHandType handCards
          Strength = getHandStrength cardValues split[0] }
      Bid = int split[1] }

// Compare two rounds, first by their type, then by their strength
let compareRounds a b =
    compare (a.Hand.Type, a.Hand.Strength) (b.Hand.Type, b.Hand.Strength)

// Rank all rounds parsed from the lines and calculate the sum of all bid,
// weighted by their round's rank
let calculateWeightedRankSum lines cards wildCards =
    lines
    |> Seq.map (parseRound cards wildCards)
    |> Seq.sortWith compareRounds
    |> Seq.mapi (fun index round -> (index + 1, round))
    |> Seq.fold (fun acc (rank, round) -> acc + rank * round.Bid) 0

// Returns a sequence containing the solution to each part in order
let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)

        yield calculateWeightedRankSum lines "23456789TJQKA" false
        yield calculateWeightedRankSum lines "J23456789TQKA" true
    }
