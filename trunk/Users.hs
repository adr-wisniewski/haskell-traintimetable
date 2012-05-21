module Users where
data User = User String String | Anonymous
u1 = User "Admin" "abc"
users = [u1]

tryLogin :: User -> Bool
tryLogin (User login password) = checkLogin login password users
tryLogin Anonymous = False

checkLogin :: String -> String -> [User] -> Bool
checkLogin _ _ [] = False
checkLogin login1 password1 ((User login2 password2):xs) = if (login1 == login2) && (password1 == password2) then True
														   else checkLogin login1 password1 xs
                                                                   