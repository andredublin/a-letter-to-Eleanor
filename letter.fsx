type Milestone =
    | Milestone of string

and Experience =
    | Love of string * milestone: Milestone
    | Joy of string * milestone: Milestone
    | Waiting of string * Milestone: Milestone
    | Future of string

and Memory =
    { Experience : Experience
      Milestone : Milestone }

type Journey =
    { Life : Map<Milestone, Experience>
      Milestone: Milestone }

let life = [
    { Milestone = Milestone "waiting"
      Experience = Waiting("We waited so long for you to arrive.", Milestone "born") }

    { Milestone = Milestone "born"
      Experience = Love("We fell in love with you the moment you came.", Milestone "home") }
    
    { Milestone = Milestone "home"
      Experience = Joy("Now that you are in our arms and home with us.", Milestone "future") }

    { Milestone = Milestone "future"
      Experience = Future("We look forward to watching you grow.") }
]

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind processFunc lastResult =
    match lastResult with
    | Success s -> processFunc s
    | Failure f -> Failure f

let (>>=) x f = bind f x

let switch processFunc input = Success (processFunc input)

// letter
let journey =
    { Milestone = Milestone "waiting"
      Life = life 
             |> Seq.map (fun milestone -> (milestone.Milestone, milestone.Experience))
             |> Map.ofSeq }

let display result =
    match result with
    | Success s -> printfn "%s" s
    | Failure f -> printfn "%s" f

let getMilestone journey milestone =
    match journey.Life.TryFind milestone with
    | Some milestone -> Success milestone
    | _ -> Failure ""

let describeExperience details = sprintf "\n%s\n" details

let extractExperience experience = 
    match experience with
    | Waiting (details, _) -> details
    | Love (details, _) -> details
    | Joy (details, _) -> details
    | Future details -> details

let describeMilestone journey =
    journey.Milestone
    |> getMilestone journey 
    |> (bind (switch extractExperience) >> bind (switch describeExperience))

let setCurrentMilestone journey milestone : Journey = { journey with Milestone = milestone }

let getCurrentMilestone journey =
    journey.Milestone
    |> getMilestone journey

let extractMilestone experience =
    match experience with
    | Waiting (_, milestone) -> Success milestone
    | Love (_, milestone) -> Success milestone
    | Joy (_, milestone) -> Success milestone
    | _ -> Failure ""
    
let progress journey = 
    journey 
    |> getCurrentMilestone >>= switch (fun experience -> experience) >>= extractMilestone >>= switch (setCurrentMilestone journey)

let applyUpdate updateFunc journey =
    match updateFunc journey with
    | Success updatedJourney ->
        describeMilestone updatedJourney |> display
        updatedJourney
    | Failure _ ->
        journey
        
type LetterProgress = 
    | UpdateMilestone of (Journey -> Result<Journey, string>)
    | FinishLetter

type Letter(beginning) =
    let loop = MailboxProcessor.Start(fun inbox -> 
        let rec innerLoop journey =
            async {
                let! message = inbox.Receive()
                match message with
                | UpdateMilestone updateFunc -> return! innerLoop (applyUpdate updateFunc journey)
                | FinishLetter -> return ()
            }

        innerLoop beginning)

    member this.WriteLetter(updateFunc) =
        loop.Post(UpdateMilestone updateFunc)

    member this.FinishLetter() =
        loop.Post(FinishLetter)

let letter = Letter(journey)

letter.WriteLetter(progress)
letter.WriteLetter(progress)
letter.WriteLetter(progress)
letter.FinishLetter()