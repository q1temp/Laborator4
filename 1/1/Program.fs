open System

type BinaryTree<'T> =
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>
    | Empty

let rnd = Random()

let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then
            Node(v, insert value left, right)
        else
            Node(v, left, insert value right)

let rec generateRandomTree nodeCount =
    if nodeCount <= 0 then Empty
    else
        let value = rnd.NextDouble() * 20.0 - 10.0
        insert value (generateRandomTree (nodeCount - 1))

let rec mapTree f tree =
    match tree with
    | Empty -> Empty
    | Node(data, left, right) ->
        Node(f data, mapTree f left, mapTree f right)

let rec printTree indent tree =
    match tree with
    | Empty -> ()
    | Node(v, l, r) ->
        printfn "%s- %A" indent v
        printTree (indent + "  ") l
        printTree (indent + "  ") r

[<EntryPoint>]
let main argv =
    let nodeCount = 10
    let root = generateRandomTree nodeCount

    printfn "Исходное дерево (вещественные числа):"
    printTree "" root

    let intTree = mapTree int root

    printfn "\nДерево после отбрасывания дробной части (целые числа):"
    printTree "" intTree

    0   