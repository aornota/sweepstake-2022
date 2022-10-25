module Aornota.Sweepstake2022.Server.DefaultData

open Aornota.Sweepstake2022.Common.Domain.Core
open Aornota.Sweepstake2022.Common.Domain.Draft
open Aornota.Sweepstake2022.Common.Domain.Fixture
open Aornota.Sweepstake2022.Common.Domain.Squad
open Aornota.Sweepstake2022.Common.Domain.User
open Aornota.Sweepstake2022.Common.IfDebug
open Aornota.Sweepstake2022.Common.WsApi.ServerMsg
open Aornota.Sweepstake2022.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2022.Server.Agents.Entities.Drafts
open Aornota.Sweepstake2022.Server.Agents.Entities.Fixtures
open Aornota.Sweepstake2022.Server.Agents.Entities.Squads
open Aornota.Sweepstake2022.Server.Agents.Entities.Users
open Aornota.Sweepstake2022.Server.Agents.Persistence
open Aornota.Sweepstake2022.Server.Authorization
open Aornota.Sweepstake2022.Server.Common.Helpers

open System
open System.IO

let private deleteExistingUsersEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingSquadsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingFixturesEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingDraftsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)

let private log category = (Host, category) |> consoleLogger.Log

let private logResult shouldSucceed scenario result =
    match shouldSucceed, result with
    | true, Ok _ -> sprintf "%s -> succeeded (as expected)" scenario |> Verbose |> log
    | true, Error error -> sprintf "%s -> unexpectedly failed -> %A" scenario error |> Danger |> log
    | false, Ok _ -> sprintf "%s -> unexpectedly succeeded" scenario |> Danger |> log
    | false, Error error -> sprintf "%s -> failed (as expected) -> %A" scenario error |> Verbose |> log
let private logShouldSucceed scenario result = result |> logResult true scenario
let private logShouldFail scenario result = result |> logResult false scenario

let private delete dir =
    Directory.GetFiles dir |> Array.iter File.Delete
    Directory.Delete dir

let private ifToken fCmdAsync token = async { return! match token with | Some token -> token |> fCmdAsync | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync }

let private superUser = SuperUser
let private nephId = Guid.Empty |> UserId
let private nephTokens = permissions nephId superUser |> UserTokens

let private qatarId = Guid "00000011-0000-0000-0000-000000000000" |> SquadId
let private ecuadorId = Guid "00000012-0000-0000-0000-000000000000" |> SquadId
let private senegalId = Guid "00000013-0000-0000-0000-000000000000" |> SquadId
let private netherlandsId = Guid "00000014-0000-0000-0000-000000000000" |> SquadId

let private englandId = Guid "00000021-0000-0000-0000-000000000000" |> SquadId
let private iranId = Guid "00000022-0000-0000-0000-000000000000" |> SquadId
let private unitedStatesId = Guid "00000023-0000-0000-0000-000000000000" |> SquadId
let private walesId = Guid "00000024-0000-0000-0000-000000000000" |> SquadId

let private argentinaId = Guid "00000031-0000-0000-0000-000000000000" |> SquadId
let private saudiArabiaId = Guid "00000032-0000-0000-0000-000000000000" |> SquadId
let private mexicoId = Guid "00000033-0000-0000-0000-000000000000" |> SquadId
let private polandId = Guid "00000034-0000-0000-0000-000000000000" |> SquadId

let private franceId = Guid "00000041-0000-0000-0000-000000000000" |> SquadId
let private australiaId = Guid "00000042-0000-0000-0000-000000000000" |> SquadId
let private denmarkId = Guid "00000043-0000-0000-0000-000000000000" |> SquadId
let private tunisiaId = Guid "00000044-0000-0000-0000-000000000000" |> SquadId

let private spainId = Guid "00000051-0000-0000-0000-000000000000" |> SquadId
let private costaRiceId = Guid "00000052-0000-0000-0000-000000000000" |> SquadId
let private germanyId = Guid "00000053-0000-0000-0000-000000000000" |> SquadId
let private japanId = Guid "00000054-0000-0000-0000-000000000000" |> SquadId

let private belgiumId = Guid "00000061-0000-0000-0000-000000000000" |> SquadId
let private canadaId = Guid "00000062-0000-0000-0000-000000000000" |> SquadId
let private morocooId = Guid "00000063-0000-0000-0000-000000000000" |> SquadId
let private croatiaId = Guid "00000064-0000-0000-0000-000000000000" |> SquadId

let private brazilId = Guid "00000071-0000-0000-0000-000000000000" |> SquadId
let private serbiaId = Guid "00000072-0000-0000-0000-000000000000" |> SquadId
let private switzerlandId = Guid "00000073-0000-0000-0000-000000000000" |> SquadId
let private cameroonId = Guid "00000074-0000-0000-0000-000000000000" |> SquadId

let private portugalId = Guid "00000081-0000-0000-0000-000000000000" |> SquadId
let private ghanaId = Guid "00000082-0000-0000-0000-000000000000" |> SquadId
let private uruguayId = Guid "00000083-0000-0000-0000-000000000000" |> SquadId
let private southKoreaId = Guid "00000084-0000-0000-0000-000000000000" |> SquadId

let private createInitialUsersEventsIfNecessary = async {
    let usersDir = directory EntityType.Users

    // Force re-creation of initial User/s events if directory already exists (if requested).
    if deleteExistingUsersEvents && Directory.Exists usersDir then
        sprintf "deleting existing User/s events -> %s" usersDir |> Info |> log
        delete usersDir

    if Directory.Exists usersDir then sprintf "preserving existing User/s events -> %s" usersDir |> Info |> log
    else
        sprintf "creating initial User/s events -> %s" usersDir |> Info |> log
        "starting Users agent" |> Info |> log
        () |> users.Start
        // Note: Send dummy OnUsersEventsRead to Users agent to ensure that it transitions [from pendingOnUsersEventsRead] to managingUsers; otherwise HandleCreateUserCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnUsersEventsRead to Users agent" |> Info |> log
        [] |> users.OnUsersEventsRead

        // Create initial SuperUser | Administators.
        let neph = UserName "neph"
        let dummyPassword = Password "password"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, nephId, neph, dummyPassword, superUser) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" neph)       
        let administrator = Administrator
        let rosieId, rosie = Guid "ffffffff-0001-0000-0000-000000000000" |> UserId, UserName "rosie"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, rosieId, rosie, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rosie)
        let hughId, hugh = Guid "ffffffff-0002-0000-0000-000000000000" |> UserId, UserName "hugh"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, hughId, hugh, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" hugh)
        let pleb = Pleb
        let chrisId, chris = Guid.NewGuid() |> UserId, UserName "chris"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, chrisId, chris, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" chris)
        let damianId, damian = Guid.NewGuid() |> UserId, UserName "damian"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, damianId, damian, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" damian)
        let denisId, denis = Guid.NewGuid() |> UserId, UserName "denis"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, denisId, denis, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" denis)
        let jemId, jem = Guid.NewGuid() |> UserId, UserName "jem"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, jemId, jem, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" jem)
        let joshId, josh = Guid.NewGuid() |> UserId, UserName "josh"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, joshId, josh, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" josh)
        let robId, rob = Guid.NewGuid() |> UserId, UserName "rob"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, robId, rob, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rob)
        let steveMId, steveM = Guid.NewGuid() |> UserId, UserName "steve m"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, steveMId, steveM, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" steveM)
        let steveSId, steveS = Guid.NewGuid() |> UserId, UserName "steve s"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, steveSId, steveS, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" steveS)
        let susieId, susie = Guid.NewGuid() |> UserId, UserName "susie"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, susieId, susie, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" susie)
        let willId, will = Guid.NewGuid() |> UserId, UserName "will"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, willId, will, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" will)
        let callumId, callum = Guid.NewGuid() |> UserId, UserName "callum"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, callumId, callum, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" callum)
        let chadId, chad = Guid.NewGuid() |> UserId, UserName "chad"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, chadId, chad, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" chad)
        let jackId, jack = Guid.NewGuid() |> UserId, UserName "jack"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, jackId, jack, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" jack)
        let martynId, martyn = Guid.NewGuid() |> UserId, UserName "martyn"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, martynId, martyn, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" martyn)
        let mikeId, mike = Guid.NewGuid() |> UserId, UserName "mike"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, mikeId, mike, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" mike)
        let sueId, sue = Guid.NewGuid() |> UserId, UserName "sue"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, sueId, sue, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" sue)
        let highnamId, highnam = Guid.NewGuid() |> UserId, UserName "highnam"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, highnamId, highnam, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" highnam)
        let mollyId, molly = Guid.NewGuid() |> UserId, UserName "molly"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, mollyId, molly, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" molly)
        let nourdineId, nourdine = Guid.NewGuid() |> UserId, UserName "nourdine"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, nourdineId, nourdine, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" nourdine)

        // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Users agent" |> Info |> log
        () |> users.Reset
    return () }

let private createInitialSquadsEventsIfNecessary = async {
    let squadsDir = directory EntityType.Squads

    // Force re-creation of initial Squad/s events if directory already exists (if requested).
    if deleteExistingSquadsEvents && Directory.Exists squadsDir then
        sprintf "deleting existing Squad/s events -> %s" squadsDir |> Info |> log
        delete squadsDir

    if Directory.Exists squadsDir then sprintf "preserving existing Squad/s events -> %s" squadsDir |> Info |> log
    else
        sprintf "creating initial Squad/s events -> %s" squadsDir |> Info |> log
        "starting Squads agent" |> Info |> log
        () |> squads.Start
        // Note: Send dummy OnSquadsEventsRead to Squads agent to ensure that it transitions [from pendingOnSquadsEventsRead] to managingSquads; otherwise HandleCreateSquadCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnSquadsEventsRead to Squads agent" |> Info |> log
        [] |> squads.OnSquadsEventsRead

        // Create initial Squads.

        // Group A
        let qatar = SquadName "Qatar"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, qatarId, qatar, GroupA, None, CoachName "Félix Sánchez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" qatar)
        let ecuador = SquadName "Ecuador"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, ecuadorId, ecuador, GroupA, None, CoachName "Gustavo Alfaro") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ecuador)
        let senegal = SquadName "Senegal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, senegalId, senegal, GroupA, Some (Seeding 16), CoachName "Aliou Cissé") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" senegal)
        let netherlands = SquadName "Netherlands"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, netherlandsId, netherlands, GroupA, Some (Seeding 9), CoachName "Louis van Gaal") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" netherlands)

        // #Group B
        let england = SquadName "England"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, englandId, england, GroupB, Some (Seeding 5), CoachName "Gareth Southgate") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" england)
        let iran = SquadName "Iran"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, iranId, iran, GroupB, None, CoachName "Carlos Queiroz") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" iran)
        let unitedStates = SquadName "United States"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, unitedStatesId, unitedStates, GroupB, Some (Seeding 14), CoachName "Gregg Berhalter") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" unitedStates)
        let wales = SquadName "Wales"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, walesId, wales, GroupB, None, CoachName "Rob Page") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" wales)

        // Group C
        let argentina = SquadName "Argentina"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, argentinaId, argentina, GroupC, Some (Seeding 4), CoachName "Lionel Scaloni") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" argentina)
        let saudiArabia = SquadName "Saudi Arabia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, saudiArabiaId, saudiArabia, GroupC, None, CoachName "Hervé Renard") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" saudiArabia)
        let mexico = SquadName "Mexico"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, mexicoId, mexico, GroupC, Some (Seeding 8), CoachName "Gerardo Martino") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" mexico)
        let poland = SquadName "Poland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, polandId, poland, GroupC, None, CoachName "Czesław Michniewicz") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" poland)

        // Group D
        let france = SquadName "France"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, franceId, france, GroupD, Some (Seeding 3), CoachName "Didier Deschamps") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" france)
        let australia = SquadName "Australia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, australiaId, australia, GroupD, None, CoachName "Graham Arnold") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" australia)
        let denmark = SquadName "Denmark"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, denmarkId, denmark, GroupD, Some (Seeding 10), CoachName "Kasper Hjulmand") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" denmark)
        let tunisia = SquadName "Tunisia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, tunisiaId, tunisia, GroupD, None, CoachName "Jalel Kadri") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" tunisia)

        // Group E
        let spain = SquadName "Spain"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, spainId, spain, GroupE, Some (Seeding 6), CoachName "Luis Enrique") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" spain)
        let costaRica = SquadName "Costa Rica"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, costaRiceId, costaRica, GroupE, None, CoachName "Luis Fernando Suárez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" costaRica)
        let germany = SquadName "Germany"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, germanyId, germany, GroupE, Some (Seeding 11), CoachName "Hansi Flick") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" germany)
        let japan = SquadName "Japan"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, japanId, japan, GroupE, None, CoachName "Hajime Moriyasu") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" japan)

        // Group F
        let belgium = SquadName "Belgium"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, belgiumId, belgium, GroupF, Some (Seeding 2), CoachName "Roberto Martínez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" belgium)
        let canada = SquadName "Canada"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, canadaId, canada, GroupF, None, CoachName "John Herdman") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" canada)
        let morocco = SquadName "Morocoo"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, morocooId, morocco, GroupF, None, CoachName "Walid Regragui") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" morocco)
        let croatia = SquadName "Croatia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, croatiaId, croatia, GroupF, Some (Seeding 15), CoachName "Zlatko Dalić") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" croatia)
        
        // Group G
        let brazil = SquadName "Brazil"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, brazilId, brazil, GroupG, Some (Seeding 1), CoachName "Tite") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" brazil)
        let serbia = SquadName "Serbia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, serbiaId, serbia, GroupG, None, CoachName "Dragan Stojković") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" serbia)
        let switzerland = SquadName "Switzerland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, switzerlandId, switzerland, GroupG, Some (Seeding 13), CoachName "Murat Yakin") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" switzerland)
        let cameroon = SquadName "Cameroon"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, cameroonId, cameroon, GroupG, None, CoachName "Rigobert Song") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" cameroon)
        
        // Group H
        let portugal = SquadName "Portugal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, portugalId, portugal, GroupH, Some (Seeding 7), CoachName "Fernando Santos") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" portugal)
        let ghana = SquadName "Ghana"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, ghanaId, ghana, GroupH, None, CoachName "Otto Addo") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ghana)
        let uruguay = SquadName "Uruguay"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, uruguayId, uruguay, GroupH, Some (Seeding 12), CoachName "Diego Alonso") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" uruguay)
        let southKorea = SquadName "South Korea"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, southKoreaId, southKorea, GroupH, None, CoachName "Paulo Bento") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" southKorea)
        
        // Note: Reset Squads agent [to pendingOnSquadsEventsRead] so that it handles subsequent SquadsEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Squads agent" |> Info |> log
        () |> squads.Reset
    return () }

let private createInitialFixturesEventsIfNecessary = async {
    let fixtureId matchNumber =
        if matchNumber < 10u then sprintf "00000000-0000-0000-0000-00000000000%i" matchNumber |> Guid |> FixtureId
        else if matchNumber < 100u then sprintf "00000000-0000-0000-0000-0000000000%i" matchNumber |> Guid |> FixtureId
        else FixtureId.Create ()

    let fixturesDir = directory EntityType.Fixtures

    // Force re-creation of initial Fixture/s events if directory already exists (if requested).
    if deleteExistingFixturesEvents && Directory.Exists fixturesDir then
        sprintf "deleting existing Fixture/s events -> %s" fixturesDir |> Info |> log
        delete fixturesDir

    if Directory.Exists fixturesDir then sprintf "preserving existing Fixture/s events -> %s" fixturesDir |> Info |> log
    else
        sprintf "creating initial Fixture/s events -> %s" fixturesDir |> Info |> log
        "starting Fixtures agent" |> Info |> log
        () |> fixtures.Start
        // Note: Send dummy OnFixturesEventsRead to Users agent to ensure that it transitions [from pendingOnFixturesEventsRead] to managingFixtures; otherwise HandleCreateFixtureCmdAsync would be ignored (and block).
        "sending dummy OnFixturesEventsRead to Fixtures agent" |> Info |> log
        [] |> fixtures.OnFixturesEventsRead

        // Group A
        let qatarVsEcuador = (2022, 11, 20, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 1u, Group GroupA, Confirmed qatarId, Confirmed ecuadorId, qatarVsEcuador) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 1u)
        
        let senegalVsNetherland = (2022, 11, 21, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 2u, Group GroupA, Confirmed senegalId, Confirmed netherlandsId, senegalVsNetherland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 2u)

        let qatarVsSenegal = (2022, 11, 25, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 18u, Group GroupA, Confirmed qatarId, Confirmed senegalId, qatarVsSenegal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 18u)

        let netherlandVsEcuador = (2022, 11, 25, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 19u, Group GroupA, Confirmed netherlandsId, Confirmed ecuadorId, netherlandVsEcuador) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 19u)

        let ecuadorVsSenegal = (2022, 11, 29, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 35u, Group GroupA, Confirmed ecuadorId, Confirmed senegalId, ecuadorVsSenegal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 35u)

        let netherlandVsQatar = (2022, 11, 29, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 36u, Group GroupA, Confirmed netherlandsId, Confirmed qatarId, netherlandVsQatar) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 36u)

        // Group B
        let englandVsIran = (2022, 11, 21, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 3u, Group GroupB, Confirmed englandId, Confirmed iranId, englandVsIran) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 3u)
        
        let unitedStatesVsWales = (2022, 11, 21, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 4u, Group GroupB, Confirmed unitedStatesId, Confirmed walesId, unitedStatesVsWales) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 4u)

        let walesVsIran = (2022, 11, 25, 10, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 17u, Group GroupB, Confirmed walesId, Confirmed iranId, walesVsIran) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 17u)

        let englandVsUnitedStates = (2022, 11, 25, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 20u, Group GroupB, Confirmed englandId, Confirmed unitedStatesId, englandVsUnitedStates) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 20u)

        let walesVsEngland = (2022, 11, 29, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 33u, Group GroupB, Confirmed walesId, Confirmed englandId, walesVsEngland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 33u)

        let iranVsUnitedStatus = (2022, 11, 29, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 34u, Group GroupB, Confirmed iranId, Confirmed unitedStatesId, iranVsUnitedStatus) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 34u)
        
        // Group C
        let argentinaVsSaudiArabia = (2022, 11, 22, 10, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 8u, Group GroupC, Confirmed argentinaId, Confirmed saudiArabiaId, argentinaVsSaudiArabia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 8u)
        
        let mexicoVsPoland = (2022, 11, 22, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 7u, Group GroupC, Confirmed mexicoId, Confirmed polandId, mexicoVsPoland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 7u)

        let polandVsSaudiArabia = (2022, 11, 26, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 22u, Group GroupC, Confirmed polandId, Confirmed saudiArabiaId, polandVsSaudiArabia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 22u)

        let argentinaVsMexico = (2022, 11, 26, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 24u, Group GroupC, Confirmed argentinaId, Confirmed mexicoId, argentinaVsMexico) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 24u)

        let polandVsArgentina = (2022, 11, 30, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 39u, Group GroupC, Confirmed polandId, Confirmed argentinaId, polandVsArgentina) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 39u)

        let saudiArabiaVsMexico = (2022, 11, 30, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 40u, Group GroupC, Confirmed saudiArabiaId, Confirmed mexicoId, saudiArabiaVsMexico) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 40u)

        // Group D
        let denmarkVsTunisia = (2022, 11, 22, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 6u, Group GroupD, Confirmed denmarkId, Confirmed tunisiaId, denmarkVsTunisia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 6u)
        
        let franceVsAustralia = (2022, 11, 22, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 5u, Group GroupD, Confirmed franceId, Confirmed australiaId, franceVsAustralia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 5u)

        let tunisiaVsAustralia = (2022, 11, 26, 10, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 21u, Group GroupD, Confirmed tunisiaId, Confirmed australiaId, tunisiaVsAustralia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 21u)

        let franceVsDenmark = (2022, 11, 26, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 23u, Group GroupD, Confirmed franceId, Confirmed denmarkId, franceVsDenmark) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 23u)

        let australiaVsDenmark = (2022, 11, 30, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 37u, Group GroupD, Confirmed australiaId, Confirmed denmarkId, australiaVsDenmark) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 37u)

        let tunisiaVsFrance = (2022, 11, 30, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 38u, Group GroupD, Confirmed tunisiaId, Confirmed franceId, tunisiaVsFrance) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 38u)

        // Group E
        let germanyVsJapan = (2022, 11, 23, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 11u, Group GroupE, Confirmed germanyId, Confirmed japanId, germanyVsJapan) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 11u)
        
        let spainVsCostaRica = (2022, 11, 23, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 10u, Group GroupE, Confirmed spainId, Confirmed costaRiceId, spainVsCostaRica) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 10u)

        let japanVsCostaRica = (2022, 11, 27, 10, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 25u, Group GroupE, Confirmed japanId, Confirmed costaRiceId, japanVsCostaRica) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 25u)

        let spainVsGermany = (2022, 11, 27, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 28u, Group GroupE, Confirmed spainId, Confirmed germanyId, spainVsGermany) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 28u)

        let japanVsSpain = (2022, 12, 01, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 43u, Group GroupE, Confirmed japanId, Confirmed spainId, japanVsSpain) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 43u)

        let costaRicaVsGermany = (2022, 12, 01, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 44u, Group GroupE, Confirmed costaRiceId, Confirmed germanyId, costaRicaVsGermany) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 44u)

        // Group F
        let moroccoVsCroatia = (2022, 11, 23, 10, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 12u, Group GroupF, Confirmed morocooId, Confirmed croatiaId, moroccoVsCroatia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 12u)
        
        let belgiumVsCanada = (2022, 11, 23, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 9u, Group GroupF, Confirmed belgiumId, Confirmed canadaId, belgiumVsCanada) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 9u)

        let belgiumVsMorocco = (2022, 11, 27, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 26u, Group GroupF, Confirmed belgiumId, Confirmed morocooId, belgiumVsMorocco) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 26u)

        let croatiaVsCanada = (2022, 11, 27, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 27u, Group GroupF, Confirmed croatiaId, Confirmed canadaId, croatiaVsCanada) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 27u)

        let croatiaVsBelgium = (2022, 12, 01, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 41u, Group GroupF, Confirmed croatiaId, Confirmed belgiumId, croatiaVsBelgium) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 41u)

        let canadaVsMorocco = (2022, 12, 01, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 42u, Group GroupF, Confirmed canadaId, Confirmed morocooId, canadaVsMorocco) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 42u)

        // Group G
        let switzerlandVsCameroon = (2022, 11, 24, 10, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 13u, Group GroupG, Confirmed switzerlandId, Confirmed cameroonId, switzerlandVsCameroon) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 13u)
        
        let brazilVsSerbia = (2022, 11, 24, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 16u, Group GroupG, Confirmed brazilId, Confirmed serbiaId, brazilVsSerbia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 16u)

        let cameroonVsSerbia = (2022, 11, 28, 10, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 29u, Group GroupG, Confirmed cameroonId, Confirmed serbiaId, cameroonVsSerbia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 29u)

        let brazilVsSwitzerland = (2022, 11, 28, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 31u, Group GroupG, Confirmed brazilId, Confirmed switzerlandId, brazilVsSwitzerland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 31u)

        let serbiaVsSwitzerland = (2022, 12, 02, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 47u, Group GroupG, Confirmed serbiaId, Confirmed switzerlandId, serbiaVsSwitzerland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 47u)

        let cameroonVsBrazil = (2022, 12, 02, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 48u, Group GroupG, Confirmed cameroonId, Confirmed brazilId, cameroonVsBrazil) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 48u)

        // Group H
        let uruguayVsSouthKorea = (2022, 11, 24, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 14u, Group GroupH, Confirmed uruguayId, Confirmed southKoreaId, uruguayVsSouthKorea) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 14u)
        
        let portugalVsGhana = (2022, 11, 24, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 15u, Group GroupH, Confirmed portugalId, Confirmed ghanaId, portugalVsGhana) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 15u)

        let southKoreaVsGhana = (2022, 11, 28, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 30u, Group GroupH, Confirmed southKoreaId, Confirmed ghanaId, southKoreaVsGhana) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 30u)

        let portugalVsUruguay = (2022, 11, 28, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 32u, Group GroupH, Confirmed portugalId, Confirmed uruguayId, portugalVsUruguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 32u)

        let ghanaVsUruguay = (2022, 12, 02, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 45u, Group GroupH, Confirmed ghanaId, Confirmed uruguayId, ghanaVsUruguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 45u)

        let southKoreaVsPortugal = (2022, 12, 02, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 46u, Group GroupH, Confirmed southKoreaId, Confirmed portugalId, southKoreaVsPortugal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 46u)

        // Round-of-16
        let winnerAVsRunnerUpBKO = (2022, 12, 03, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 49u, RoundOf16 49u, Unconfirmed (Winner (Group GroupA)), Unconfirmed (RunnerUp GroupB), winnerAVsRunnerUpBKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 49u)

        let winnerCVsRunnerUpDKO = (2022, 12, 03, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 50u, RoundOf16 50u, Unconfirmed (Winner (Group GroupC)), Unconfirmed (RunnerUp GroupD), winnerCVsRunnerUpDKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 50u)

        let winnerDVsRunnerUpCKO = (2022, 12, 04, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 52u, RoundOf16 52u, Unconfirmed (Winner (Group GroupD)), Unconfirmed (RunnerUp GroupC), winnerDVsRunnerUpCKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 52u)

        let winnerBVsRunnerUpAKO = (2022, 12, 04, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 51u, RoundOf16 51u, Unconfirmed (Winner (Group GroupB)), Unconfirmed (RunnerUp GroupA), winnerBVsRunnerUpAKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 51u)

        let winnerEVsRunnerUpFKO = (2022, 12, 05, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 53u, RoundOf16 53u, Unconfirmed (Winner (Group GroupE)), Unconfirmed (RunnerUp GroupF), winnerEVsRunnerUpFKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 53u)

        let winnerGVsRunnerUpHKO = (2022, 12, 05, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 54u, RoundOf16 54u, Unconfirmed (Winner (Group GroupG)), Unconfirmed (RunnerUp GroupH), winnerGVsRunnerUpHKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 54u)

        let winnerFVsRunnerUpEKO = (2022, 12, 06, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 55u, RoundOf16 55u, Unconfirmed (Winner (Group GroupF)), Unconfirmed (RunnerUp GroupE), winnerFVsRunnerUpEKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 55u)

        let winnerHVsRunnerUpGKO = (2022, 12, 06, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 56u, RoundOf16 56u, Unconfirmed (Winner (Group GroupH)), Unconfirmed (RunnerUp GroupG), winnerHVsRunnerUpGKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 56u)

        // Quarter-finals
        let winner53VsWinner54KO = (2022, 12, 09, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 58u, QuarterFinal 1u, Unconfirmed (Winner (RoundOf16 53u)), Unconfirmed (Winner (RoundOf16 54u)), winner53VsWinner54KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 58u)

        let winner49VsWinner50KO = (2022, 12, 09, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 57u, QuarterFinal 2u, Unconfirmed (Winner (RoundOf16 49u)), Unconfirmed (Winner (RoundOf16 50u)), winner49VsWinner50KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 57u)

        let winner55VsWinner56KO = (2022, 12, 10, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 60u, QuarterFinal 3u, Unconfirmed (Winner (RoundOf16 55u)), Unconfirmed (Winner (RoundOf16 56u)), winner55VsWinner56KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 60u)

        let winner51VsWinner52KO = (2022, 12, 10, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 59u, QuarterFinal 4u, Unconfirmed (Winner (RoundOf16 51u)), Unconfirmed (Winner (RoundOf16 52u)), winner51VsWinner52KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 59u)

        // Semi-finals
        let winnerQF2VsWinnerQF1KO = (2022, 12, 13, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 61u, SemiFinal 1u, Unconfirmed (Winner (QuarterFinal 2u)), Unconfirmed (Winner (QuarterFinal 1u)), winnerQF2VsWinnerQF1KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 61u)

        let winnerQF4VsWinnerQF3KO = (2022, 12, 14, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 62u, SemiFinal 2u, Unconfirmed (Winner (QuarterFinal 4u)), Unconfirmed (Winner (QuarterFinal 3u)), winnerQF4VsWinnerQF3KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 62u)

        // Third place play-off
        let loserSF1VsLoserSF2KO = (2022, 12, 17, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 63u, ThirdPlacePlayOff, Unconfirmed (Loser (SemiFinal 1u)), Unconfirmed (Loser (SemiFinal 2u)), loserSF1VsLoserSF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 63u)

        // Final
        let winnerSF1VsWinnerSF2KO = (2022, 12, 18, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 64u, Final, Unconfirmed (Winner (SemiFinal 1u)), Unconfirmed (Winner (SemiFinal 2u)), winnerSF1VsWinnerSF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 64u)

        // Note: Reset Fixtures agent [to pendingOnFixturesEventsRead] so that it handles subsequent FixturesEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Fixtures agent" |> Info |> log
        () |> fixtures.Reset
    return () }

let private createInitialDraftsEventsIfNecessary = async {
    let draftsDir = directory EntityType.Drafts

    // Force re-creation of initial Draft/s events if directory already exists (if requested).
    if deleteExistingDraftsEvents && Directory.Exists draftsDir then
        sprintf "deleting existing Draft/s events -> %s" draftsDir |> Info |> log
        delete draftsDir

    if Directory.Exists draftsDir then sprintf "preserving existing Draft/s events -> %s" draftsDir |> Info |> log
    else
        sprintf "creating initial Draft/s events -> %s" draftsDir |> Info |> log
        "starting Drafts agent" |> Info |> log
        () |> drafts.Start
        // Note: Send dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent to ensure that it transitions [from pendingAllRead] to managingDrafts; otherwise HandleCreateDraftCmdAsync would be ignored (and block).
        "sending dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent" |> Info |> log
        [] |> drafts.OnSquadsRead
        [] |> drafts.OnDraftsEventsRead
        [] |> drafts.OnUserDraftsEventsRead

        let draft1Id, draft1Ordinal = Guid "00000000-0000-0000-0000-000000000001" |> DraftId, DraftOrdinal 1
        let draft1Starts, draft1Ends = (2022, 11, 07, 09, 00) |> dateTimeOffsetUtc, (2022, 11, 15, 21, 00) |> dateTimeOffsetUtc
        let draft1Type = (draft1Starts, draft1Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft1Id, draft1Ordinal, draft1Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft1Id draft1Ordinal)

        let draft2Id, draft2Ordinal = Guid "00000000-0000-0000-0000-000000000002" |> DraftId, DraftOrdinal 2
        let draft2Starts, draft2Ends = (2022, 11, 16, 09, 00) |> dateTimeOffsetUtc, (2022, 11, 20, 12, 00) |> dateTimeOffsetUtc
        let draft2Type = (draft2Starts, draft2Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft2Id, draft2Ordinal, draft2Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft2Id draft2Ordinal)

        let draft3Id, draft3Ordinal = Guid "00000000-0000-0000-0000-000000000003" |> DraftId, DraftOrdinal 3
        let draft3Type = Unconstrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft3Id, draft3Ordinal, draft3Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft3Id draft3Ordinal)

        // Note: Reset Drafts agent [to pendingAllRead] so that it handles subsequent DraftsEventsRead event (&c.) appropriately (i.e. from readPersistedEvents).
        "resetting Drafts agent" |> Info |> log
        () |> drafts.Reset
    return () }

let createInitialPersistedEventsIfNecessary = async {
    "creating initial persisted events (if necessary)" |> Info |> log
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createInitialPersistedEventsIfNecessary", function | Host -> allCategories | Entity _ -> allExceptVerbose | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    do! createInitialUsersEventsIfNecessary // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialSquadsEventsIfNecessary // note: although this can cause various events to be broadcast (SquadsRead | SquadEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialFixturesEventsIfNecessary // note: although this can cause various events to be broadcast (FixturesRead | FixtureEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialDraftsEventsIfNecessary // note: although this can cause various events to be broadcast (DraftsRead | DraftEventWritten | &c.), no agents should yet be subscribed to these
    previousLogFilter |> consoleLogger.ChangeLogFilter }
