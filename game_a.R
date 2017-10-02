##########
# Game A #
##########

# Separate game file into different responses
game.a<-subset(game, !is.na(choice.a) & user!="zonination")[,c(3, 5)]

#Perform a check for duplicate entries
# game.a[duplicated(game.a$user)|duplicated(game.a$user, fromLast=T),]

# Check to see if there are even or odd columns.
# I'll enter to complete the pairing if there is odd number.
if(nrow(game.a) %% 2 != 0){
  game.a<-rbind(game.a, subset(game, !is.na(choice.a) & user=="zonination")[,c(3, 5)])}

# Begin by randomizing the sample set, and matching opponent.
game.a<-sample_n(game.a, nrow(game.a))
a<-game.a[1:(nrow(game.a)/2),]
b<-game.a[(nrow(game.a)/2 + 1):nrow(game.a),]
game.a<-cbind(a,b); rm(a); rm(b)
names(game.a)<-c("a.user", "a.choice",
                 "b.user", "b.choice")

# What is our end result?
game.a$end<-NA # Clear column
# Map out one of our four scenarios
for(n in 1:nrow(game.a)){
  # Both choose safe
  if(game.a$a.choice[n] == "Swerve" &
     game.a$b.choice[n] == "Swerve"){
    game.a$end[n] <- paste("Both /u/", game.a$a.user[n]," and /u/", game.a$b.user[n], " safely swerve.", sep="")}
  
  # A chooses risky and B chooses safe
  if(game.a$a.choice[n] == "Hit the gas" &
     game.a$b.choice[n] == "Swerve"){
    game.a$end[n] <- paste("/u/", game.a$a.user[n]," gets to call /u/", game.a$b.user[n], " a chicken.", sep="")}
  
  # A chooses safe and B chooses risky
  if(game.a$a.choice[n] == "Swerve" &
     game.a$b.choice[n] == "Hit the gas"){
    game.a$end[n] <- paste("/u/", game.a$b.user[n]," gets to call /u/", game.a$a.user[n], " a chicken.", sep="")}
  
  # Both choose risky
  if(game.a$a.choice[n] == "Hit the gas" &
     game.a$b.choice[n] == "Hit the gas"){
    game.a$end[n] <- paste("Both /u/", game.a$a.user[n]," and /u/", game.a$b.user[n], " crash and die.", sep="")}
}; rm(n)

# Save the file; and easy formatting for reddit tables
write_delim(game.a, "game_a.txt", delim="|")
write_csv(game.a, "game_a.csv")

# Plot and save the results
df<-data.frame("y"=c("Hit the gas","Swerve"), "x"=c("Swerve","Hit the gas"))
ggplot(game.a, aes(x=a.choice, y=b.choice))+
  geom_path(data=df, aes(x,y, group=1), lty=2)+
  geom_point(stat="bin2d", aes(size=..count..), shape=21, color="black", fill="slateblue1")+
  geom_text(stat="bin2d", aes(label=..count..))+
  scale_size(range=c(0,25), limits=c(0,120))+
  guides(size=F)+
  labs(title="A Game of Chicken",
       subtitle="Result matrix of scenario",
       x="Subject A",y="Subject B",
       caption="created by /u/zonination")+
  z_theme(); rm(df)
ggsave("game_a.png", height=4, width=5, dpi=120, type="cairo-png")