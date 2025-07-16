import requests
import datetime.(date, datetime)
import time
import sys

slice = class:
    __init__ = (self, start, stop, step) =>
        self.start = start
        self.stop = stop
        self.step = step

    __iter__ = self =>
        step = if self.step == None: 1 else: self.step
        start = if self.start == None: 0 else: self.start

        if self.stop == None:
            for i in range(start, 100000, step):
                @i
        else:
            @@range(start, self.stop, step)

        None

countdown = target_dt =>
    print(f"Countdown to sunset at {format(target_dt.strftime("%-I:%M:%s %p"))}")
    for i in ..:
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
    api_url = f"https://api.sunrise-sunset.org/json?lat={lat}&lng={lng}&date={today_str}&formatted=0"

    print(f"Getting sunset time for {today_str}...")

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

