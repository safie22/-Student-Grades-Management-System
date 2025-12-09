namespace StudentGradesSystem

type Role = Admin | Viewer

type User = {
    Username: string
    Role: Role
}

module Permissions =
    let AddStudent (user: User) = user.Role = Admin
    let EditStudent (user: User) = user.Role = Admin
    let DeleteStudent (user: User) = user.Role = Admin
    
    let ViewStudents (user: User) = true
    let ViewGrades (user: User) = true
    let ViewAverages (user: User) = true
    let ViewStatistics (user: User) = true
    
    let SaveData (user: User) = user.Role = Admin
    let LoadData (user: User) = true

module Authentication =
    let mutable private currentUser: User option = None
    
    let login (username: string) (password: string) : Result<User, string> =
        if username.ToLower() = "admin" && password = "admin123" then
            let user = { Username = username; Role = Admin }
            currentUser <- Some user
            Ok user
        elif password.Length >= 4 then
            let user = { Username = username; Role = Viewer }
            currentUser <- Some user
            Ok user
        else
            Error "Invalid credentials"
    
    let logout () =
        match currentUser with
        | Some user -> 
            printfn "User '%s' logged out." user.Username
            currentUser <- None
        | None -> ()
    
    let getCurrentUser () = currentUser
    
    let isLoggedIn () = currentUser.IsSome

module AccessControl =
    let executeWithPermission (user: User) (permission: User -> bool) (operation: unit -> 'T) =
        if permission user then
            try
                Ok (operation ())
            with ex -> 
                Error $"Operation failed: {ex.Message}"
        else
            Error "Access Denied"
    
    let requireAdmin (user: User) =
        if user.Role = Admin then Ok ()
        else Error "Only Admin can do that."
    
    let getRoleName (role: Role) =
        match role with
        | Admin -> "Administrator"
        | Viewer -> "Viewer"
    
    let getUserPermissions (user: User) =
        match user.Role with
        | Admin -> 
            ["Add Student"; "Edit Student"; "Delete Student"; 
             "View Students"; "View Grades"; "View Averages"; 
             "View Statistics"; "Save Data"; "Load Data"]
        | Viewer -> 
            ["View Students"; "View Grades"; "View Averages"; 
             "View Statistics"; "Load Data"]
