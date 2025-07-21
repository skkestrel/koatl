API_RESPONSE = [
    "user_a": [
        "profile": [
            "name": "Alice"
            "contact": ["email": "alice@example.com", "phone": None]
        ]
        "orders": [["id": 1, "total": 100], ["id": 2, "total": 150]]
    ]
    "user_b": [
        "profile": ["name": "Bob"] # "contact" is missing
        "orders": []
    ]
    "user_c": [
        "profile": None
    ]
]

map_err = f => value => value.(is_ok) then value.(f) else value

get_user_details = (data, user_id) =>
    user = @(try data[user_id] except KeyError()
        | map_err(e => KeyError(f"User {user_id} not found.")))

    profile = try user["profile"]

    profile!is_ok() then:
        print(f"Profile not found.")
    else:
        print(f"Name: {try profile["name"] ?? "N/A"}")

        contact = try profile["contact"]

        contact!is_ok() then:
            print(f"Contact not found: {profile}")
        else:
            print(f"Email: {try contact["email"] ?? "N/A"}")
            print(f"Phone: {try contact["phone"] ?? "N/A"}")

    orders = try user["orders"] ?? []
    n_orders = @(try len(orders) | map_err(e => ValueError("Orders is malformed.")))

    print(
        n_orders > 0 then:
            f"Number of orders: {n_orders}"
        else:
            "No orders found."
    )

    return True
  | do()

get_user_details(API_RESPONSE, "user_a") | repr | print
print("-" * 20)
get_user_details(API_RESPONSE, "user_b") | repr | print
print("-" * 20)
get_user_details(API_RESPONSE, "user_c") | repr | print
print("-" * 20)
get_user_details(API_RESPONSE, "user_d") | repr | print