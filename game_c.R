##########
# Game C #
##########

# Separate game file into different responses
game.c<-subset(game, !is.na(choice.c) & user!="zonination")[,c(3, 7)]

# Perform a check for duplicate entries
# game.c[duplicated(game.c$user)|duplicated(game.c$user, fromLast=T),]

# Check to see if there are even or odd columns.
# I'll enter to complete the pairing if there is odd number.
if(nrow(game.c) %% 2 != 0){
  game.c<-rbind(game.c, subset(game, !is.na(choice.c) & user=="zonination")[,c(3, 7)])}

# Begin by randomizing the sample set, and matching opponent.
game.c<-sample_n(game.c, nrow(game.c))
a<-game.c[1:(nrow(game.c)/2),]
b<-game.c[(nrow(game.c)/2 + 1):nrow(game.c),]
game.c<-cbind(a,b); rm(a); rm(b)
names(game.c)<-c("a.user", "a.choice",
                 "b.user", "b.choice")

# What is our end result?
game.c$end<-NA # Clear column
# Map out one of our four scenarios
for(n in 1:nrow(game.c)){
  # Both choose safe
  if(game.c$a.choice[n] == "SHARE" &
     game.c$b.choice[n] == "SHARE"){
    game.c$end[n] <- paste("Both /u/", game.c$a.user[n]," and /u/", game.c$b.user[n], " share $500 for trusting one another.", sep="")}
  
  # A chooses risky and B chooses safe
  if(game.c$a.choice[n] == "STEAL" &
     game.c$b.choice[n] == "SHARE"){
    game.c$end[n] <- paste("/u/", game.c$a.user[n]," wins $1000 for stealing from /u/", game.c$b.user[n], ", who gets nothing.", sep="")}
  
  # A chooses safe and B chooses risky
  if(game.c$a.choice[n] == "SHARE" &
     game.c$b.choice[n] == "STEAL"){
    game.c$end[n] <- paste("/u/", game.c$b.user[n]," wins $1000 for stealing from /u/", game.c$a.user[n], ", who gets nothing.", sep="")}
  
  # Both choose risky
  if(game.c$a.choice[n] == "STEAL" &
     game.c$b.choice[n] == "STEAL"){
    game.c$end[n] <- paste("Both /u/", game.c$a.user[n]," and /u/", game.c$b.user[n], " get nothing.", sep="")}
}; rm(n)

# Save the file; easy formatting for reddit tables
write_delim(game.c, "game_c.txt", delim="|")
write_csv(game.c, "game_c.csv")

# Plot and save the results
df<-data.frame("y"=c("STEAL","SHARE"), "x"=c("SHARE","STEAL"))
ggplot(game.c, aes(x=a.choice, y=b.choice))+
  geom_path(data=df, aes(x,y, group=1), lty=2)+
  geom_point(stat="bin2d", aes(size=..count..), shape=21, color="black", fill="chartreuse3")+
  geom_text(stat="bin2d", aes(label=..count..))+
  scale_size(range=c(0,25), limits=c(0,120))+
  guides(size=F)+
  labs(title="The Game Show",
       subtitle="Result matrix of scenario",
       x="Subject A",y="Subject B",
       caption="created by /u/zonination")+
  z_theme()
ggsave("game_c.png", height=4, width=5, dpi=120, type="cairo-png")