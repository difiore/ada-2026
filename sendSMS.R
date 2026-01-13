```{r}
library(emayili)
```
Common US gateways (examples):

  AT&T: number@txt.att.net
Verizon: number@vtext.com
T-Mobile: number@tmomail.net
(Confirm current gateways; they can change and may have limits.)

```{r}
smtp_gmail <- server(
  host = "smtp.gmail.com",
  port = 465,
  max_times = 1,
  username = "anthony.difiore@utexas.edu",
  password = pwd_gmail
)

msg <- envelope() %>%
  from("anthony.difiore@utexas.edu") |>
  to("6464004285@tmomail.net") |>   # Replace with recipient’s carrier gateway
  subject("") |> # Usually ignored
  text("Hello from R from my UT gmail account via email-to-SMS!")

smtp_gmail(msg)
```
