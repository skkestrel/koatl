API_RESPONSE = {
    user_a: {
        profile: {
            name: "Alice"
            contact: {email: "alice@example.com", phone: None}
        }
        orders: [{"id": 1, "total": 100}, {"id": 2, "total": 150}]
    }
    user_b: {
        profile: {name: "Bob"} # "contact" is missing
        orders: []
    }
    user_c: {
        profile: None
    }
}

get_user_details = Ok.do((data, user_id) =>
    x1 = try data[user_id] except KeyError()

    user = @(try data[user_id] except KeyError()
        | map_err(e => KeyError(f"User {user_id} not found: {e}")))

    profile = try user.profile

    not Ok(profile) then:
        print(f"Profile not found.")
    else:
        print(f"Name: {try profile.name ?? "N/A"}")

        contact = try profile.contact

        not Ok(contact) then:
            print(f"Contact not found: {profile}")
        else:
            print(f"Email: {try contact.email ?? "N/A"}")
            print(f"Phone: {try contact.phone ?? "N/A"}")

    orders = try user.orders ?? []
    n_orders = @(try len(orders) | map_err(e => ValueError("Orders is malformed.")))

    print(
        n_orders > 0 then:
            f"Number of orders: {n_orders}"
        else:
            "No orders found."
    )

    ()
)

["user_a", "user_b", "user_c", "user_d"]
    | Iterable.map($, id =>
        print(f"Searching for id {id}")
        get_user_details(API_RESPONSE, id) match:
            Ok() => print("Details fetched successfully.")
            e => print(f"Error fetching details: {repr(e)}")
        print("-" * 20)
      )
    | list