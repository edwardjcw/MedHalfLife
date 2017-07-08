module Help

type Help() =

    static let help t =
        let intro = 
            @"Medicines take a certain amount of time to reach full concentration
            in the body. At that point concentration falls by half every so often.
            That amount of time is known as the medicine's half life."
        let purpose =
            @"This program lets you keep track of and test concentration amounts
            of a medicine in the body. It currently doesn't support bioavailability,
            which is the amount of drug available to the system. So instead, it
            treats medicine as 100% available. Unless the medicine is that high
            with bioavailability, it's best to treat the concentration amounts as
            unit-less measures. After all, only a percentage of it is available."
        let instructions =
            @"Command line options are '-load' '-load [savefilename]' and '-?'.
            A default savefile is used if you don't specify the savefilename.
            The question mark brings up this screen. And no options doesn't
            save anything when you exit."
        let internalCommands=
            @"Inside the program, these commands are available:
            help, test, add, remove, list, and reset. To find out about these commands,
            enter help [command], where [command] is one of the commands."
        let helpCommand =
            @"help by itself brings up the full help document, as if you used
            -? as a command argument. help [command] tells you about a command."
        let testCommand =
            @"test gives you the concentration(s). It has four versions:
            1. test by itself gives you the current concentration, if doses exist.
            2. test [dose] gives you the current concentration with that added dose.
            3. test [time] gives you the concentration at that time.
            4. test [dose] [time] gives you the concentration at a specified time.
            [dose] is in the format [time]=[amount]
            [time] is either full datetime or just time (and current date is used)
            [amount] is the amount of the dose
            NOTE: multiple doses and multiple times are allowed, separate by space"
        let addCommand =
            @"add [dose] is the equivalent of taking the dose.
            [dose] is in the format [time]=[amount]
            [time] is either full datetime or just time (and current date is used)
            [amount] is the amount of the dose
            NOTE: multiple doses are allowed, separate by space"
        let removeCommand =
            @"remove takes away a dose as if it weren't taken. Two versions:
            1. remove [dose] finds the dose with that dose's time and removes it.
            2. remove [time] finds the dose with that time and removes it.
            [dose] is in the format [time]=[amount]
            [time] is either full datetime or just time (and current date is used)
            [amount] is the amount of the dose
            NOTE: multiple doses are allowed, separate by space"
        let listCommand =
            @"list shows the list of doses currently taken"
        let resetCommand =
            @"reset clears all the doses taken."
        let exampleCommand =
            @"example [command] provides examples for the test, add, and remove commands."
        let whenMaxCommand =
            @"whenmax determines the max concentration amount and time based on current doses.
            1. whenmax by itself determines the max amount and time on current doses.
            2. whenmax [dose] determines the max amount and time with the dose added in.
            [dose] is in the format [time]=[amount]
            [time] is either full datetime or just time (and current date is used)
            [amount] is the amount of the dose
            NOTE: multiple doses are allowed, separate by space"
        match t with
        | "" -> 
            [intro; 
            purpose; 
            instructions; 
            internalCommands; 
            helpCommand; 
            testCommand; 
            addCommand; 
            removeCommand; 
            listCommand; 
            resetCommand; 
            exampleCommand;
            whenMaxCommand] 
            |> String.concat "\n"
        | "list" -> listCommand
        | "remove" -> removeCommand
        | "add" -> addCommand
        | "test" -> testCommand
        | "help" -> helpCommand
        | "reset" -> resetCommand
        | "example" -> exampleCommand
        | "whenmax" -> whenMaxCommand
        | _ -> "invalid help command."

    static let example e =
        let testExamples = 
            [@"test 10:50=20         
            ::: This command calculates concentration right now if a 20 dose is added at 10:50 am today.";
            @"test 10:50=20 11:25   
            ::: This command calculates concentration at 11:25 am today if that 20 dose is added.";
            "test \"7/7/2017 10:50 am\"=20 \"7/8/2017 11:25 pm\"   
            ::: This command calculates for 11:25 pm on 7/8/17 if that 20 dose is added for the day before.";
            @"test 10:50=20 11:20=15 12:10=5
            ::: This command calculates concentration right if all those doses were added.";
            @"test 10:50=20 11:20=15 12:10 14:14
            ::: This command calculates concentration at those two times if those two doses are added.";
            @"test
            ::: This command calculates concentration right now based on current doses in list."]
        let addExamples =
            [@"add 10:50=20         
            ::: This command adds a dose of 20 for 10:50 am today.";
            @"add 10:50=20 11:20=15 12:10=5
            ::: This command adds three doses (20,15,and 5) for those three times today."]
        let removeExamples =
            [@"remove 10:50
            ::: This command removes any dose that has a time of 10:50 today.";
            @"remove 10:50=20
            ::: This command removes ANY dose that has a time of 10:50 today.";
            @"remove 10:50 12:50
            ::: This command removes any dose with those two times of today.";
            "remove \"7/7/2017 10:50\"
            ::: This command removes any dose that at that time and date."]
        let whenMaxExamples = [
            @"whenmax 10:50=20
            ::: This command determine the max concentration amount and time as if dose added in.";
            @"whenmax
            ::: This command determine the max concentration amount and time using just current doses."]
        let makeReady x = x |> String.concat "\n"
        match e with
        | "test" -> testExamples |> makeReady
        | "add" -> addExamples |> makeReady
        | "remove" -> removeExamples |> makeReady
        | "whenmax" -> whenMaxExamples |> makeReady
        | _ -> "no examples available"

    static member Example e =
        example e |> printfn "%A"

    static member Show t =
        help t |> printfn "%A"
