import requests
import datetime.(date, datetime)
import time
import sys

range = sl =>
    import builtins.range

    if sl.stop == None:
        range(sl.start, 1000000, 1)
    else:
        range(sl.start, sl.stop, sl.step)

countdown = target_dt =>
    print("Countdown to sunset at {}".format(target_dt.strftime("%-I:%M:%s %p")))
    print(range(0..))
    for i in range(0..):
        remaining = target_dt - datetime.now(target_dt.tzinfo)
        if remaining.total_seconds() <= 0:
            print("The sun has set!")
            break
        
        print("Time remaining: {}".format(str(remaining).split(".")[0]))
        time.sleep(1)

if __name__ == "__main__":
    lat = 49.1666
    lng = -123.1336
    today_str = date.today().isoformat()
    api_url = "https://api.sunrise-sunset.org/json?lat={lat}&lng={lng}&date={today_str}&formatted=0"
        .format(
            lat=lat,
            lng=lng,
            today_str=today_str
        )

    print("Getting sunset time for {today_str}...".format(today_str=today_str))

    response = requests.get(api_url)
    response.raise_for_status()
    data = response.json()
    
    if data["status"] == "OK":
        sunset_utc = datetime.fromisoformat(data["results"]["sunset"])
        sunset_local = sunset_utc.astimezone(tz=None)

        print("Success! Sunset is at {}.".format(
            sunset_local.strftime("%-I:%M %p local time")
        ))
        sunset_local.(countdown)
    else:
        print("API Error")

