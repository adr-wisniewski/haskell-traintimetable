module Users where
data User = User String String
u1 = User "Admin" "abc"
users = [u1]

login :: User -> IO Bool
login (User login password) = checkLogin login password users

checkLogin :: String -> String -> [User] -> IO Bool
checkLogin _ _ [] = return False
checkLogin login1 password1 ((User login2 password2):xs) = if (login1 == login2) && (password1 == password2) then return True
														   else checkLogin login1 password1 xs
                                                                   