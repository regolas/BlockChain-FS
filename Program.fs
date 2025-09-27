// For more information see https://aka.ms/fsharp-console-apps
open System.Security.Cryptography
open System.Linq
open System.IO
open System.Collections
open System
open System.Collections.Generic

type IBlock =
    abstract member Data: byte[] with get
    abstract member Hash: byte[] with get, set
    abstract member Nonce: int with get, set
    abstract member PrevHash: byte[] with get, set
    abstract member TimeStamp: DateTime with get, set

type Block (data : byte[]) =
// Why is the setting hash to Array.empty failing

    let mutable hash = Array.empty<byte>
    let mutable nonce = 0
    let mutable prevHash = [|0uy|]
    let mutable timeStamp = DateTime.Now
    
    interface IBlock with
        member this.Data = 
            if data = null then raise (ArgumentNullException("data")) else data // ArgumentNullException handling
        member this.Hash
            with get() = hash
            and set(value) = hash <- value// Initialize to empty array
        member this.Nonce
            with get() = nonce
            and set(value) = nonce <- value
        member this.PrevHash
            with get() = prevHash
            and set(value) = prevHash <- value
        member this.TimeStamp
            with get() = timeStamp
            and set(value) = timeStamp <- value

    override this.ToString() =
        let hashString = BitConverter.ToString(hash).Replace("-", "")
        let prevHashString = BitConverter.ToString(prevHash).Replace("-", "")
        $"{hashString} :\n {prevHashString} :\n {nonce} {timeStamp}"

[<AbstractClass; Sealed>]
type BlockExtension =
    static member GenerateHash (data : byte[], prevHash : byte[], nonce:int,timeStamp:DateTime) =
        let mutable sha512 = SHA512.Create()
        let ms = new MemoryStream()
        let bw = new BinaryWriter(ms)
        bw.Write(data)
        bw.Write(prevHash)
        bw.Write(nonce)
        bw.Write(timeStamp.Ticks)
        let s = ms.ToArray()
        sha512.ComputeHash(s)

    static member MineHash (block: IBlock, difficulty: byte[]) =
        let mutable hash:byte[] = Array.empty<byte>
        let maxIterations = Int32.MaxValue
        let mutable iterations = 0
        let mutable currentNonce = block.Nonce // Start with the block's current nonce

        if difficulty = null then raise (ArgumentNullException(nameof(difficulty)))
        if difficulty.Length > 32 then raise (ArgumentException("Difficulty is too long"))

        while not (hash.Take(difficulty.Length).SequenceEqual(difficulty)) do // Use difficulty.Length
            if iterations >= maxIterations then
                raise (InvalidOperationException("Max iterations reached. Mining failed."))

            currentNonce <- currentNonce + 1
            block.Nonce <- currentNonce // Update the nonce of the input block
            hash <- BlockExtension.GenerateHash(block.Data, block.PrevHash, block.Nonce, block.TimeStamp)
            block.Hash <- hash // Update the hash of the input block
            iterations <- iterations + 1
        hash // Return the found hash (though the block's hash is also updated)
    
    static member IsValid(data: byte[], prevHash: byte[], nonce: int, timeStamp: DateTime, expectedHash: byte[]) : bool =
        let generatedHash = BlockExtension.GenerateHash(data, prevHash, nonce, timeStamp)
        generatedHash.SequenceEqual(expectedHash)
    
type BlockChain(difficulty: byte[], genesis: IBlock) =
    let mutable items = List.empty<IBlock>
    do
        genesis.Hash <- BlockExtension.MineHash(genesis, difficulty)
        items <- [genesis] // Ensure items is initialized with the genesis block
            
    member this.Difficulty = difficulty

    member this.Add(item: IBlock) =
        match items with
        | [] -> raise (InvalidOperationException("Blockchain is not initialized with a genesis block."))
        | lastBlock :: _ ->
            item.PrevHash <- lastBlock.Hash
            item.Hash <- BlockExtension.MineHash(item, difficulty)
            items <- item :: items

    member this.Items
        with get() = items
        and set(value) = items <- value

    member this.Count = List.length items

    member this.Item
        with get(index: int) = List.item index items
        and set(index: int) (value: IBlock) = 
            let mutable itemsArray = items |> List.toArray
            itemsArray.[index] <- value
            items <- itemsArray |> List.ofArray
    
    // Get the last block in the chain
    member this.LastBlock =
        match items with
        | [] -> None
        | head :: _ -> Some head
    
    // Check if the blockchain is valid
    member this.IsValid() =
        let rec validateChain (blocks: IBlock list) =
            match blocks with
            | [] | [_] -> true // Empty chain or single block is valid
            | current :: next :: rest -> // 'current' is the block being validated, 'next' is the previous one
                //printfn "Validating block with Hash: %s" (BitConverter.ToString(current.Hash).Replace("-", ""))
                //printfn "  Current PrevHash: %s" (BitConverter.ToString(current.PrevHash).Replace("-", ""))
                //printfn "  Expected PrevHash (Hash of next block): %s" (BitConverter.ToString(next.Hash).Replace("-", ""))
                let prevHashMatches = current.PrevHash.SequenceEqual(next.Hash)
                //printfn "  Previous Hash Matches: %b" prevHashMatches

                let generatedHash = BlockExtension.GenerateHash(current.Data, current.PrevHash, current.Nonce, current.TimeStamp)
                let hashMatches = generatedHash.SequenceEqual(current.Hash)
                //printfn "  Generated Hash: %s" (BitConverter.ToString(generatedHash).Replace("-", ""))
                //printfn "  Current Hash Matches Expected: %b" hashMatches

                if not prevHashMatches then
                    printfn "  ERROR: Previous hash mismatch!"
                    false
                elif not hashMatches then
                    printfn "  ERROR: Hash mismatch for current block!"
                    false
                else
                    validateChain (next :: rest) // Move to the next pair of blocks
        
        match items with
        | [] -> true // An empty chain is considered valid
        | _ :: rest -> validateChain items

    interface IEnumerable<IBlock> with
        member this.GetEnumerator() : IEnumerator<IBlock> =
            let seq = items |> Seq.ofList
            seq.GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            let seq = items |> Seq.ofList
            (seq :> IEnumerable).GetEnumerator()

[<EntryPoint>]
let main argv =
    printfn "Hello from F#"
    let ran = Random()
    let genesis: IBlock = Block([|0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy|])
    let difficulty = [|0x00uy; 0x00uy|]
    let chain = BlockChain(difficulty, genesis)

    for i in 0 .. 199 do
        let data = Enumerable.Range(0, 255).Select(fun _ -> byte (ran.Next())).ToArray()
        chain.Add(Block(data))
        match chain.LastBlock with
        | Some block -> printfn "%s" (block.ToString())
        | None -> printfn "No blocks in chain"
        
        if chain.IsValid() then
            printfn "BlockChain is valid"
        else
            printfn "Chain is invalid"
    
    0 // Return an integer exit code
